(** Uid Modules *)

module Scheduler_uid = struct
  type t = int

  let equal = Int.equal
  let __current__ = Atomic.make 0
  let next () = Atomic.fetch_and_add __current__ 1
  let reset () = Atomic.set __current__ 0
end

module Pid = struct
  type t = Scheduler_uid.t * int

  let __current__ = Atomic.make 0
  let equal (s1, p1) (s2, p2) = Scheduler_uid.equal s1 s2 && Int.equal p1 p2
  let next sch () = (sch, Atomic.fetch_and_add __current__ 1)
end

(** Process *)

exception Process_killed

type state = Pending | Waiting_receive | Running | Exited of exn

type 'msg process = {
  pid : Pid.t;
  runner : Scheduler_uid.t;
  state : state Atomic.t;
  mailbox : 'msg Queue.t;
  links : Pid.t list;
  monitors : Pid.t list;
}

and pack = Pack : 'a process -> pack

module Process = struct
  module Pid = Pid

  let kill proc =
    match Atomic.get proc.state with
    | Pending | Waiting_receive | Running ->
        Atomic.set proc.state (Exited Process_killed)
    | Exited _ -> ()

  let is_alive proc =
    match Atomic.get proc.state with
    | Running | Waiting_receive -> true
    | _ -> false

  let make ~runner =
    {
      pid = Pid.next runner ();
      runner;
      state = Atomic.make Pending;
      links = [];
      monitors = [];
      mailbox = Queue.create ();
    }

  let pid { pid; _ } = pid
end

(** Exceptions *)

exception No_scheduler_available

(** Effects *)

[@@@warning "-30"]

type _ Effect.t += Spawn : Scheduler_uid.t * (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Kill : Pid.t -> unit Effect.t
type _ Effect.t += Link : Pid.t -> unit Effect.t
type _ Effect.t += Monitor : Pid.t -> unit Effect.t
type _ Effect.t += Send : ('msg * 'msg process) -> unit Effect.t
type _ Effect.t += Receive : 'msg process -> 'msg option Effect.t
type _ Effect.t += Schedulers : Scheduler_uid.t list Effect.t
type _ Effect.t += Scheduler_self : Scheduler_uid.t Effect.t
type _ Effect.t += Scheduler_count : int Effect.t
type _ Effect.t += Random : Random.State.t Effect.t

(** Scheduler *)
type task =
  | Tick : task
  | Arrived : 'msg process * (unit -> unit) -> task
  | Killed : 'msg process -> task
  | Suspended : 'msg process -> task

and scheduler = {
  uid : Scheduler_uid.t;
  rnd : Random.State.t;
  tasks : (int * task) Heapq.t;
  system_tasks : (int, unit) Hashtbl.t;
  (* a monotonically incremented tick used to order tasks *)
  tick : int Atomic.t;
}

and pool_task =
  | Task : 'a process * (unit -> unit) -> pool_task
  | Kill : 'a process -> pool_task

and pool = {
  mutable stop : bool;
  mutable working_counter : int;
  condition_pending_work: Condition.t;
  condition_all_idle: Condition.t;
  mutex : Mutex.t;
  tasks : pool_task Sequence.t;
  schedulers : scheduler list;
}

module Scheduler = struct
  module Uid = Scheduler_uid

  module Task = struct
    let to_int = function
      | Killed _ -> 0
      | Arrived _ -> 1
      | Suspended _ -> 2
      | Tick -> 3

    let compare a b = to_int a - to_int b
  end

  let make ~rnd () =
    let uid = Uid.next () in
    (* NOTE(@ostera): we define this comparison function here to allow
       scheduler tasks to be sorted automatically by the HeapQueue, so that if
       2 tasks are the same, we use their (monotonic) tick to decide which
       one goes further up.

       This effectively sorts our tasks in the order we want to run them.
    *)
    let compare (tick0, a) (tick1, b) =
      let order = Task.compare a b in
      if order = 0 then Int.compare tick0 tick1 else order
    in
    {
      rnd = Random.State.copy rnd;
      uid;
      tasks = Heapq.create ~compare ~dummy:(0, Tick) 1_000;
      system_tasks = Hashtbl.create 10;
      tick = Atomic.make 0;
    }

  let add_task t task =
    let tick = Atomic.fetch_and_add t.tick 1 in
    Heapq.add t.tasks (tick, task)

  let handle _pool _t _proc = ()

  let once pool t task =
    match task with
    | Tick -> Domain.cpu_relax ()
    | Arrived (proc, _fn) -> handle pool t proc
    | Killed _ -> ()
    | Suspended _ -> ()

  let system_tasks_suspended t = Hashtbl.length t.system_tasks > 0
  let unblock_awaits_with_system_tasks _pool _t = ()

  let one_task_for (scheduler: scheduler) (pool: pool) =
    let exception Yes in
    let f =
      function
      | Kill proc when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace Yes
      | Task (proc, _fn) when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace Yes
      | _ -> ()
    in
    try Sequence.iter ~f pool.tasks; false with
    | Yes -> true

  let is_idle t = Heapq.is_empty t.tasks

  let run pool t () =
    match Heapq.pop_minimum t.tasks with
    | exception Heapq.Empty ->
        if system_tasks_suspended t then unblock_awaits_with_system_tasks pool t
    | _tick, elt ->
        once pool t elt;
        if system_tasks_suspended t then unblock_awaits_with_system_tasks pool t

  let self () = Effect.perform Scheduler_self
  let count () = Effect.perform Scheduler_count
  let list () = Effect.perform Schedulers
end

module Pool = struct
  let nothing_to_do (pool: pool) (scheduler: scheduler) =
    (Scheduler.is_idle scheduler)
    && Scheduler.system_tasks_suspended scheduler = false
    && Scheduler.one_task_for scheduler pool = false

  let rec transfer_all_tasks (pool : pool) (scheduler : scheduler) =
    let exception Task of pool_task Sequence.node in
    let f node =
      match Sequence.data node with
      | Kill proc when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace (Task node)
      | Task (proc, _fn) when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace (Task node)
      | _ -> ()
    in

    try Sequence.iter_node ~f pool.tasks
    with Task node ->
      let data = Sequence.data node in
      Sequence.remove node;
      (match data with
      | Kill proc -> Scheduler.add_task scheduler (Killed proc)
      | Task (proc, fn) -> Scheduler.add_task scheduler (Arrived (proc, fn)));
      transfer_all_tasks pool scheduler

  let worker pool scheduler () =
    let exception Exit in
    try
      while true do
        Mutex.lock pool.mutex;

        while nothing_to_do pool scheduler && not pool.stop do
          Condition.wait pool.condition_pending_work pool.mutex
        done;

        if pool.stop then raise_notrace Exit;
        transfer_all_tasks pool scheduler;
        pool.working_counter <- pool.working_counter + 1;
        Mutex.unlock pool.mutex;

        Scheduler.run pool scheduler ();

        Mutex.lock pool.mutex;
        pool.working_counter <- pool.working_counter - 1;
        Mutex.unlock pool.mutex
      done
    with Exit ->
      pool.working_counter <- pool.working_counter - 1;
      Condition.signal pool.condition_all_idle;
      Mutex.unlock pool.mutex

  let kill pool =
    Mutex.lock pool.mutex;
    pool.stop <- true;
    Mutex.unlock pool.mutex

  let make ?(rnd = Random.State.make_self_init ())
      ?(domains = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) () =
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in
    let pool =
      {
        mutex = Mutex.create ();
        tasks = Sequence.create rnd;
        working_counter = 0;
        stop = false;
        schedulers;
      }
    in
    let spawn scheduler =
      Stdlib.Domain.spawn (fun () -> worker pool scheduler ())
    in
    (pool, List.map spawn schedulers)
end

let random () = Effect.perform Random

(** Public API *)

let spawn fn =
  let schedulers = Scheduler.list () in
  if schedulers = [] then raise No_scheduler_available;
  let curr = Scheduler.self () in
  let scheduler_id =
    let rnd = random () in
    let other_schedulers =
      List.filter (fun sch -> not (Scheduler_uid.equal curr sch)) schedulers
    in
    List.nth other_schedulers
      (Random.State.int rnd (List.length other_schedulers))
  in
  Effect.perform (Spawn (scheduler_id, fn))

let run ?(rnd = Random.State.make_self_init ()) main =
  Scheduler.Uid.reset ();
  let sch0 = Scheduler.make ~rnd () in
  let proc = Process.make ~runner:sch0.uid in
  Scheduler.add_task sch0 (Arrived (proc, main));
  let pool, domains = Pool.make () in
  Scheduler.run pool sch0 ();
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  Ok ()
