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
  let newer (s1, p1) (s2, p2) = s1 == s2 && p1 > p2
  let next sch () = (sch, Atomic.fetch_and_add __current__ 1)
  let scheduler (sch, _) = sch
  let proc_id (_, proc_id) = proc_id

  let pp : Format.formatter -> t -> unit =
   fun ppf (sch, pid) -> Format.fprintf ppf "<%d.%d>" sch pid
end

module Message = struct
  type select_marker = Take | Skip
  type t = ..
  type t += Exit_signal
end

module Mailbox = struct
  type t = Message.t Queue.t

  let queue t msg = Queue.add msg t
  let next (t : t) = Queue.take_opt t
  let is_empty (t : t) = Queue.is_empty t
  let create () = Queue.create ()
  let merge (a : t) (b : t) = Queue.add_seq a (Queue.to_seq b)
end

(** Process *)

exception Process_killed

type state = Pending | Waiting_receive | Running | Exited of exn

type process = {
  pid : Pid.t;
  runner : Scheduler_uid.t;
  state : state Atomic.t;
  mailbox : Mailbox.t;
  links : Pid.t list;
  monitors : Pid.t list;
}
(** ['msg process] an internal process descriptor. Represents a process in the runtime. *)

(** [exit_reason] indicates why a process was terminated. *)
and exit_reason = Normal | Exit_signal | Timeout_value | Bad_link

(** [signal]s are used to communicate to a process that a given action needs to be performed by it. They are low-level primitives used by the runtime. *)
and signal = Link | Unlink | Exit | Monitor | Demonitor | Message

and pack = Pack : process -> pack

let pp_signal ppf t =
  match t with
  | Link -> Format.fprintf ppf "Link"
  | Unlink -> Format.fprintf ppf "Unlink"
  | Exit -> Format.fprintf ppf "Exit"
  | Monitor -> Format.fprintf ppf "Monitor"
  | Demonitor -> Format.fprintf ppf "Demonitor"
  | Message -> Format.fprintf ppf "Message"

module Process = struct
  type t = process

  module Pid = Pid

  let pack proc = Pack proc

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
      mailbox = Mailbox.create ();
    }

  let pid { pid; _ } = pid
end

module Process_table = struct
  type t = { processes : (Pid.t, Process.t) Hashtbl.t }

  let create () = { processes = Hashtbl.create 256_000 }
  let register_process t proc = Hashtbl.add t.processes proc.pid proc
  let get t pid = Hashtbl.find_opt t.processes pid
end

(** Exceptions *)

exception No_scheduler_available

(** Effects *)

[@@@warning "-30"]

type _ Effect.t +=
  | Link : Pid.t -> unit Effect.t
  | Monitor : Pid.t -> unit Effect.t
  | Random : Random.State.t Effect.t
  | Receive : {
      select : Message.t -> Message.select_marker;
    }
      -> Message.t Effect.t
  | Scheduler_count : int Effect.t
  | Scheduler_self : Scheduler_uid.t Effect.t
  | Schedulers : Scheduler_uid.t list Effect.t
  | Self : Pid.t Effect.t
  | Send : (Message.t * Pid.t) -> unit Effect.t
  | Signal : Pid.t -> unit Effect.t
  | Spawn : Scheduler_uid.t * (unit -> unit) -> Pid.t Effect.t
  | Yield : unit Effect.t

let pp_effect : type a. Format.formatter -> a Effect.t -> unit =
 fun ppf eff ->
  match eff with
  | Link _ -> Format.fprintf ppf "Link"
  | Monitor _ -> Format.fprintf ppf "Monitor"
  | Random -> Format.fprintf ppf "Random"
  | Receive _ -> Format.fprintf ppf "Receive"
  | Scheduler_count -> Format.fprintf ppf "Schedulers_count"
  | Scheduler_self -> Format.fprintf ppf "Schedulers_self"
  | Schedulers -> Format.fprintf ppf "Schedulers"
  | Self -> Format.fprintf ppf "Self"
  | Send _ -> Format.fprintf ppf "Send"
  | Signal _ -> Format.fprintf ppf "Signal"
  | Spawn _ -> Format.fprintf ppf "Spawn"
  | Yield -> Format.fprintf ppf "Yield"
  | _effect -> Format.fprintf ppf "Unhandled effect"

(** Scheduler *)
type task =
  | Tick : task
  | Arrived : process * (unit -> exit_reason) -> task
  | Signaled : Pid.t * signal -> task
  | Terminated : process * (exit_reason, exn) result -> task
  | Suspended : process * exit_reason Proc_state.t -> task
  | Deliver_message : Pid.t * Message.t -> task

module Task = struct
  let to_int = function
    | Terminated _ -> 0
    | Arrived _ -> 1
    | Deliver_message _ -> 2
    | Signaled _ -> 3
    | Suspended _ -> 4
    | Tick -> 5

  let compare a b = to_int a - to_int b
end

module Task_queue = struct
  type t = (int * task) Queue.t

  (* NOTE(@ostera): we define this comparison function here to allow
     scheduler tasks to be sorted automatically by the HeapQueue, so that if
     2 tasks are the same, we use their (monotonic) tick to decide which
     one goes further up.

     This effectively sorts our tasks in the order we want to run them.
  *)
  let compare (tick0, a) (tick1, b) =
    let order = Task.compare a b in
    if order = 0 then Int.compare tick0 tick1 else order

  let create () = Queue.create ()
  let add t task = Queue.add task t
  let is_empty t = Queue.is_empty t

  let next t =
    match Queue.pop t with exception Queue.Empty -> None | task -> Some task
end

type scheduler = {
  uid : Scheduler_uid.t;
  rnd : Random.State.t;
  tasks : Task_queue.t;
  system_tasks : (int, unit) Hashtbl.t;
  (* a monotonically incremented tick used to order tasks *)
  tick : int Atomic.t;
  processes : Process_table.t;
}

and pool_task =
  | Remote_spawn : { proc : process; fn : unit -> unit } -> pool_task
  | Remote_signal : { signal : signal; proc : Pid.t } -> pool_task
  | Deliver_message : { message : Message.t; proc : Pid.t } -> pool_task

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
  | Signaled (pid, signal) ->
      Format.fprintf ppf "Signaled(%a <- %a)" Pid.pp pid pp_signal signal
  | Terminated (proc, _) -> Format.fprintf ppf "Terminated(%a)" Pid.pp proc.pid
  | Suspended (proc, _) -> Format.fprintf ppf "Suspended(%a)" Pid.pp proc.pid
  | Deliver_message (pid, _) ->
      Format.fprintf ppf "Deliver_message(%a)" Pid.pp pid

and pp_process ppf t =
  let sch, pid = t.pid in
  Format.fprintf ppf "pid=<%d.%d>" sch pid

let pp_pool_task ppf (t : pool_task) =
  match t with
  | Remote_spawn { proc; _ } ->
      Format.fprintf ppf "Remote_spawn(%a)" Pid.pp proc.pid
  | Remote_signal { signal; proc } ->
      Format.fprintf ppf "Remote_signal([%a]-> %a)" pp_signal signal Pid.pp proc
  | Deliver_message { proc; _ } ->
      Format.fprintf ppf "Deliver_message(%a)" Pid.pp proc

let arrived proc fn =
  Arrived
    ( proc,
      fun () ->
        fn ();
        Normal )

module Scheduler = struct
  module Uid = Scheduler_uid

  let make ~rnd () =
    let uid = Uid.next () in
    Logs.debug (fun f -> f "Making scheduler with id: %d" uid);
    {
      rnd = Random.State.copy rnd;
      uid;
      tasks = Task_queue.create ();
      system_tasks = Hashtbl.create 10;
      tick = Atomic.make 0;
      processes = Process_table.create ();
    }

  let add_into_pool pool scheduler (task : pool_task) =
    let tick = Atomic.fetch_and_add scheduler.tick 1 in
    Logs.debug (fun f ->
        f "[tick=%d] Adding remote task: %a" tick pp_pool_task task);
    Mutex.lock pool.mutex;
    (match task with
    | Deliver_message _ -> Sequence.add_r task pool.tasks
    | Remote_signal _ -> Sequence.add_r task pool.tasks
    | Remote_spawn _ -> Sequence.add_r task pool.tasks);
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex

  let add_task t task =
    let tick = Atomic.fetch_and_add t.tick 1 in
    Logs.debug (fun f ->
        f "[scheduler=%d,tick=%d] Adding task: %a" t.uid tick pp_task task);
    Task_queue.add t.tasks (tick, task)

  let perform pool scheduler (Pack process) =
    let open Proc_state in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.debug (fun f -> f "performing effect: %a" pp_effect eff);
      match eff with
      | Random -> k (Continue scheduler.rnd)
      | Yield -> k Yield
      | Schedulers ->
          let uids = List.map (fun { uid; _ } -> uid) pool.schedulers in
          k (Continue uids)
      | Scheduler_self -> k (Continue scheduler.uid)
      | Scheduler_count -> k (Continue (List.length pool.schedulers))
      | Self -> k (Continue (Process.pid process))
      | Send (msg, pid) ->
          if Pid.scheduler pid == scheduler.uid then
            add_task scheduler (Deliver_message (pid, msg))
          else
            add_into_pool pool scheduler
              (Deliver_message { message = msg; proc = pid });
          k (Continue ())
      (* NOTE(leostera): the selective receive algorithm goes:

         * is there a new message?
           -> no: reperform – we will essentially be blocked here until we
                  either receive a message or we timeout (if a timeout is set)
           -> yes: check if we should take the message
              -> take: return the message and continue
              -> skip: put the message on a temporary skip queue
         * loop until the mailbox is
      *)
      | Receive { select } as effect ->
          let skipped = Mailbox.create () in
          let rec go () =
            (* NOTE(leostera): we can get the value out of the option because
               the case above checks for an empty mailbox. *)
            match Mailbox.next process.mailbox with
            | None ->
                Mailbox.merge process.mailbox skipped;
                k (Delay effect)
            | Some msg -> (
                match select msg with
                | Take -> k (Continue msg)
                | Skip ->
                    Mailbox.queue skipped msg;
                    go ())
          in
          go ()
      | Spawn (runner, fn) ->
          let proc = Process.make ~runner in
          if Scheduler_uid.equal runner scheduler.uid then
            add_task scheduler (arrived proc fn)
          else add_into_pool pool scheduler (Remote_spawn { proc; fn });
          k (Continue proc.pid)
      | effect -> k (Reperform effect)
    in
    { perform }

  let handle _pool scheduler proc proc_state =
    Logs.debug (fun f ->
        f "handling process state: %a" Proc_state.pp proc_state);
    match proc_state with
    | Proc_state.Finished reason ->
        add_task scheduler (Terminated (proc, reason))
    | Proc_state.Suspended _ as state ->
        add_task scheduler (Suspended (proc, state))
    | Proc_state.Unhandled _ as state ->
        add_task scheduler (Suspended (proc, state))

  let once pool scheduler task =
    Logs.debug (fun f -> f "executing task: %a" pp_task task);
    match task with
    | Tick -> Domain.cpu_relax ()
    | Arrived (proc, fn) ->
        Process_table.register_process scheduler.processes proc;
        handle pool scheduler proc (Proc_state.make fn)
    | Terminated _ -> ()
    | Signaled (_proc, _signal) -> ()
    | Deliver_message (pid, msg) -> (
        match Process_table.get scheduler.processes pid with
        | Some proc ->
            Logs.info (fun f -> f "delivering message to %a" Pid.pp pid);
            Mailbox.queue proc.mailbox msg;
            add_task scheduler (Signaled (pid, Message))
        | None ->
            Logs.info (fun f -> f "COULD NOT DELIVER message to %a" Pid.pp pid))
    | Suspended (proc, proc_state) ->
        let perform = perform pool scheduler (Process.pack proc) in
        let state = Proc_state.run ~perform proc_state in
        handle pool scheduler proc state

  let system_tasks_suspended t = Hashtbl.length t.system_tasks > 0
  let unblock_awaits_with_system_tasks _pool _t = ()

  let one_task_for (scheduler : scheduler) (pool : pool) =
    let exception Yes in
    let f : pool_task -> unit = function
      | Remote_signal { proc; _ }
        when Scheduler_uid.equal (Pid.scheduler proc) scheduler.uid ->
          raise_notrace Yes
      | Remote_spawn { proc; _ }
        when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace Yes
      | _ -> ()
    in
    try
      Sequence.iter ~f pool.tasks;
      false
    with Yes -> true

  let is_idle t = Task_queue.is_empty t.tasks

  let step pool t () =
    match Task_queue.next t.tasks with
    | None ->
        Logs.debug (fun f -> f "no tasks");
        if system_tasks_suspended t then unblock_awaits_with_system_tasks pool t
    | Some (_tick, task) ->
        Logs.debug (fun f -> f "found task: %a" pp_task task);
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
      | Remote_spawn { proc; _ }
        when Scheduler_uid.equal proc.runner scheduler.uid ->
          raise_notrace (Task node)
      | (Deliver_message { proc; _ } | Remote_signal { proc; _ })
        when Scheduler_uid.equal (Pid.scheduler proc) scheduler.uid ->
          raise_notrace (Task node)
      | _ -> ()
    in

    try Sequence.iter_node ~f pool.tasks
    with Task node ->
      let data = Sequence.data node in
      Sequence.remove node;
      Logs.debug (fun f -> f "transfering task: %a" pp_pool_task data);
      (match data with
      (* TODO(@leostera): should we do this inside the Scheduler instead? *)
      | Deliver_message { message; proc = pid } ->
          Scheduler.add_task scheduler (Deliver_message (pid, message));
          Scheduler.add_task scheduler (Signaled (pid, Message))
      | Remote_signal { proc; signal = Exit } -> (
          match Process_table.get scheduler.processes proc with
          | Some proc ->
              Scheduler.add_task scheduler (Terminated (proc, Ok Exit_signal))
          | None -> ())
      | Remote_signal { proc; signal } ->
          Scheduler.add_task scheduler (Signaled (proc, signal))
      | Remote_spawn { proc; fn } ->
          Scheduler.add_task scheduler (arrived proc fn));
      transfer_all_tasks pool scheduler

  let worker pool scheduler () =
    Logs.debug (fun f -> f "> enter worker loop");
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
    Logs.debug (fun f -> f "< exit worker loop")

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

let receive ?(select = fun _ -> Message.Take) () =
  Effect.perform (Receive { select })

let send pid msg = Effect.perform (Send (msg, pid))
let yield () = Effect.perform Yield

let spawn fn =
  let schedulers = Scheduler.list () in
  if schedulers = [] then raise No_scheduler_available;
  let scheduler_id =
    let rnd = random () in
    List.nth schedulers (Random.State.int rnd (List.length schedulers))
  in
  Effect.perform (Spawn (scheduler_id, fn))

let run ?(rnd = Random.State.make_self_init ()) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Scheduler.Uid.reset ();
  let sch0 = Scheduler.make ~rnd () in
  let proc = Process.make ~runner:sch0.uid in
  Scheduler.add_task sch0 (arrived proc main);
  let pool, domains = Pool.make ~main:sch0 () in
  Pool.worker pool sch0 ();
  Logs.debug (fun f -> f "Riot runtime shutting down...");
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  Logs.debug (fun f -> f "Riot runtime shutdown");
  Ok ()
