module Proc_state = Proc_state
module Logs = Logs

(** Uid Modules *)

module Scheduler_uid = struct
  type t = int

  let equal = Int.equal
  let __current__ = Atomic.make 0
  let next () = Atomic.fetch_and_add __current__ 1

  let reset () =
    Logs.debug (fun f -> f "Resetting Scheduler Uids");
    Atomic.set __current__ 0
end

module Pid = struct
  type t = Scheduler_uid.t * int

  let __current__ = Atomic.make 0
  let equal (s1, p1) (s2, p2) = Scheduler_uid.equal s1 s2 && Int.equal p1 p2
  let next sch () = (sch, Atomic.fetch_and_add __current__ 1)

  let pp : Format.formatter -> t -> unit =
   fun ppf (sch, pid) -> Format.fprintf ppf "<%d.%d>" sch pid
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
(** ['msg process] an internal process descriptor. Represents a process in the runtime. *)

(** [exit_reason] indicates why a process was terminated. *)
and exit_reason = Normal | Exit_signal | Timeout_value | Bad_link

(** [signal]s are used to communicate to a process that a given action needs to be performed by it. They are low-level primitives used by the runtime. *)
and signal = Link | Unlink | Exit | Monitor | Demonitor | Message

and pack = Pack : 'a process -> pack

module Process = struct
  module Pid = Pid

  let pack : type a. a process -> pack = fun proc -> Pack proc

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
    let pid = Pid.next runner () in
    Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
    {
      pid;
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

type _ Effect.t += Spawn : Scheduler_uid.t * (unit -> unit) -> Pid.t Effect.t
type _ Effect.t += Self : Pid.t Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Signal : Pid.t -> unit Effect.t
type _ Effect.t += Link : Pid.t -> unit Effect.t
type _ Effect.t += Monitor : Pid.t -> unit Effect.t
type _ Effect.t += Send : ('msg * 'msg process) -> unit Effect.t
type _ Effect.t += Receive : 'msg process -> 'msg option Effect.t
type _ Effect.t += Schedulers : Scheduler_uid.t list Effect.t
type _ Effect.t += Scheduler_self : Scheduler_uid.t Effect.t
type _ Effect.t += Scheduler_count : int Effect.t
type _ Effect.t += Random : Random.State.t Effect.t

let pp_effect : type a. Format.formatter -> a Effect.t -> unit =
 fun ppf eff ->
  match eff with
  | Random -> Format.fprintf ppf "Random"
  | Yield -> Format.fprintf ppf "Yield"
  | Schedulers -> Format.fprintf ppf "Schedulers"
  | Scheduler_self -> Format.fprintf ppf "Schedulers_self"
  | Scheduler_count -> Format.fprintf ppf "Schedulers_count"
  | _effect -> Format.fprintf ppf "Unhandled effect"

(** Scheduler *)
type task =
  | Tick : task
  | Arrived : 'msg process * (unit -> exit_reason) -> task
  | Signaled : 'msg process * signal -> task
  | Terminated : 'msg process * (exit_reason, exn) result -> task
  | Suspended : 'msg process * exit_reason Proc_state.t -> task

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
  | Signal : 'a process * signal -> pool_task

and pool = {
  mutable stop : bool;
  mutable working_counter : int;
  condition_pending_work : Condition.t;
  condition_all_idle : Condition.t;
  mutex : Mutex.t;
  tasks : pool_task Sequence.t;
  schedulers : scheduler list;
}

let rec pp_task ppf (t : task) =
  match t with
  | Tick -> Format.fprintf ppf "Tick"
  | Arrived (proc, _) -> Format.fprintf ppf "Arrived(%a)" pp_process proc
  | Signaled (_, _) -> Format.fprintf ppf "Sinaled"
  | Terminated (_, _) -> Format.fprintf ppf "Terminated"
  | Suspended (_, _) -> Format.fprintf ppf "Suspended"

and pp_process : type a. Format.formatter -> a process -> unit =
 fun ppf t ->
  let sch, pid = t.pid in
  Format.fprintf ppf "pid=<%d.%d>" sch pid

let arrived proc fn =
  Arrived
    ( proc,
      fun () ->
        fn ();
        Normal )

module Scheduler = struct
  module Uid = Scheduler_uid

  module Task = struct
    let to_int = function
      | Terminated _ -> 0
      | Signaled _ -> 1
      | Arrived _ -> 2
      | Suspended _ -> 3
      | Tick -> 4

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
    Logs.debug (fun f -> f "Making scheduler with id: %d" uid);
    {
      rnd = Random.State.copy rnd;
      uid;
      tasks = Heapq.create ~compare ~dummy:(0, Tick) 1_000;
      system_tasks = Hashtbl.create 10;
      tick = Atomic.make 0;
    }

  let add_task t task =
    let tick = Atomic.fetch_and_add t.tick 1 in
    Logs.debug (fun f ->
        f "[scheduler=%d,tick=%d] Adding task: %a" t.uid tick pp_task task);
    Heapq.add t.tasks (tick, task)

  let perform pool scheduler (Pack proc) =
    let open Proc_state in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.debug (fun f ->
          f "[scheduler=%d] performing effect: %a" scheduler.uid pp_effect eff);
      match eff with
      | Random -> k (Continue scheduler.rnd)
      | Yield -> k Yield
      | Schedulers ->
          let uids = List.map (fun { uid; _ } -> uid) pool.schedulers in
          k (Continue uids)
      | Scheduler_self -> k (Continue scheduler.uid)
      | Scheduler_count -> k (Continue (List.length pool.schedulers))
      | Self -> k (Continue proc.pid)
      | Spawn (runner, fn) ->
          let process = Process.make ~runner in
          add_task scheduler (arrived process fn);
          k (Continue process.pid)
      | effect -> k (Reperform effect)
    in
    { perform }

  let handle _pool scheduler proc proc_state =
    Logs.debug (fun f ->
        f "[scheduler=%d] handling process state: %a" scheduler.uid
          Proc_state.pp proc_state);
    match proc_state with
    | Proc_state.Finished reason ->
        add_task scheduler (Terminated (proc, reason))
    | Proc_state.Suspended _ as state ->
        add_task scheduler (Suspended (proc, state))
    | Proc_state.Unhandled _ as state ->
        add_task scheduler (Suspended (proc, state))

  let once pool scheduler task =
    Logs.debug (fun f ->
        f "[scheduler=%d] executing task: %a" scheduler.uid pp_task task);
    match task with
    | Tick -> Domain.cpu_relax ()
    | Arrived (proc, fn) -> handle pool scheduler proc (Proc_state.make fn)
    | Terminated _ -> ()
    | Signaled (_proc, _signal) -> ()
    | Suspended (proc, proc_state) ->
        let perform = perform pool scheduler (Process.pack proc) in
        let state = Proc_state.run ~perform proc_state in
        handle pool scheduler proc state

  let system_tasks_suspended t = Hashtbl.length t.system_tasks > 0
  let unblock_awaits_with_system_tasks _pool _t = ()

  let one_task_for (scheduler : scheduler) (pool : pool) =
    let exception Yes in
    let f = function
      | Signal (proc, _signal)
        when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace Yes
      | Task (proc, _fn) when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace Yes
      | _ -> ()
    in
    try
      Sequence.iter ~f pool.tasks;
      false
    with Yes -> true

  let is_idle t = Heapq.is_empty t.tasks

  let step pool t () =
    match Heapq.pop_minimum t.tasks with
    | exception Heapq.Empty ->
        Logs.debug (fun f -> f "[scheduler=%d] no tasks" t.uid);
        if system_tasks_suspended t then unblock_awaits_with_system_tasks pool t
    | _tick, task ->
        Logs.debug (fun f ->
            f "[scheduler=%d] found task: %a" t.uid pp_task task);
        once pool t task;
        if system_tasks_suspended t then unblock_awaits_with_system_tasks pool t

  let self () = Effect.perform Scheduler_self
  let count () = Effect.perform Scheduler_count
  let list () = Effect.perform Schedulers
end

module Pool = struct
  let nothing_to_do (pool : pool) (scheduler : scheduler) =
    Scheduler.is_idle scheduler
    && Scheduler.system_tasks_suspended scheduler = false
    && Scheduler.one_task_for scheduler pool = false

  let rec transfer_all_tasks (pool : pool) (scheduler : scheduler) =
    let exception Task of pool_task Sequence.node in
    let f node =
      match Sequence.data node with
      | Signal (proc, _signal)
        when Scheduler_uid.equal proc.runner scheduler.uid ->
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
      (* TODO(@leostera): should we do this inside the Scheduler instead? *)
      | Signal (proc, Exit) ->
          Scheduler.add_task scheduler (Terminated (proc, Ok Exit_signal))
      | Signal (proc, signal) ->
          Scheduler.add_task scheduler (Signaled (proc, signal))
      | Task (proc, fn) -> Scheduler.add_task scheduler (arrived proc fn));
      transfer_all_tasks pool scheduler

  let worker pool scheduler () =
    Logs.debug (fun f -> f "[scheduler=%d] > enter worker loop" scheduler.uid);
    let exception Exit in
    (try
       while true do
         Mutex.lock pool.mutex;

         while nothing_to_do pool scheduler && not pool.stop do
           Condition.wait pool.condition_pending_work pool.mutex
         done;

         if pool.stop then raise_notrace Exit;
         transfer_all_tasks pool scheduler;
         pool.working_counter <- pool.working_counter + 1;
         Mutex.unlock pool.mutex;

         Scheduler.step pool scheduler ();

         Mutex.lock pool.mutex;
         pool.working_counter <- pool.working_counter - 1;
         if (not pool.stop) && Int.equal pool.working_counter 0 then
           Condition.signal pool.condition_all_idle;
         Mutex.unlock pool.mutex
       done
     with Exit ->
       pool.working_counter <- pool.working_counter - 1;
       Condition.signal pool.condition_all_idle;
       Mutex.unlock pool.mutex);
    Logs.debug (fun f -> f "[scheduler=%d] < exit worker loop" scheduler.uid)

  let kill pool =
    Mutex.lock pool.mutex;
    pool.stop <- true;
    Mutex.unlock pool.mutex;
    Logs.debug (fun f -> f "killed scheduler pool")

  let make ?(rnd = Random.State.make_self_init ())
      ?(domains = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) ~main
      () =
    Logs.debug (fun f -> f "Making scheduler pool...");
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in
    let pool =
      {
        mutex = Mutex.create ();
        tasks = Sequence.create rnd;
        working_counter = 0;
        stop = false;
        schedulers = [ main ] @ schedulers;
        condition_pending_work = Condition.create ();
        condition_all_idle = Condition.create ();
      }
    in
    let spawn scheduler =
      Stdlib.Domain.spawn (fun () -> worker pool scheduler ())
    in
    Logs.debug (fun f -> f "Created %d schedulers" (List.length schedulers));
    (pool, List.map spawn schedulers)
end

let random () = Effect.perform Random

(** Public API *)

let self () = Effect.perform Self

let spawn fn =
  Logs.debug (fun f -> f "> Spawning process");
  let schedulers = Scheduler.list () in
  if schedulers = [] then raise No_scheduler_available;
  Logs.debug (fun f -> f "> found schedulers");
  let scheduler_id = Scheduler.self () in
  Logs.debug (fun f -> f "> Spawning process on scheduler=%d" scheduler_id);
  Effect.perform (Spawn (scheduler_id, fn))

let run ?(rnd = Random.State.make_self_init ()) main =
  Logs.info (fun f -> f "Initializing Riot runtime...");
  Scheduler.Uid.reset ();
  let sch0 = Scheduler.make ~rnd () in
  let proc = Process.make ~runner:sch0.uid in
  Scheduler.add_task sch0 (arrived proc main);
  let pool, domains = Pool.make ~domains:0 ~main:sch0 () in
  Pool.worker pool sch0 ();
  Logs.debug (fun f -> f "Riot runtime shutting down...");
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  Logs.info (fun f -> f "Riot runtime shutdown");
  Ok ()
