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
  type t = int

  let __current__ = Atomic.make 0
  let equal (s1, p1) (s2, p2) = Scheduler_uid.equal s1 s2 && Int.equal p1 p2
  let newer (s1, p1) (s2, p2) = s1 == s2 && p1 > p2
  let next () = Atomic.fetch_and_add __current__ 1

  let pp : Format.formatter -> t -> unit =
   fun ppf pid -> Format.fprintf ppf "<0.%d.0>" pid
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

  let is_awaiting_message proc =
    match Atomic.get proc.state with Waiting_receive -> true | _ -> false

  let state t = Atomic.get t.state
  let set_pending proc = Atomic.set proc.state Pending
  let set_waiting_receive proc = Atomic.set proc.state Waiting_receive

  let make () =
    let pid = Pid.next () in
    Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
    {
      pid;
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
  | Spawn : (unit -> exit_reason) -> Pid.t Effect.t
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
  | Remote_spawn : { proc : process; fn : unit -> exit_reason } -> pool_task
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

and pp_process ppf t = Format.fprintf ppf "pid=%a" Pid.pp t.pid

let pp_pool_task ppf (t : pool_task) =
  match t with
  | Remote_spawn { proc; _ } ->
      Format.fprintf ppf "Remote_spawn(%a)" Pid.pp proc.pid
  | Remote_signal { signal; proc } ->
      Format.fprintf ppf "Remote_signal([%a]-> %a)" pp_signal signal Pid.pp proc
  | Deliver_message { proc; _ } ->
      Format.fprintf ppf "Deliver_message(%a)" Pid.pp proc

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

  let add_task t task =
    let tick = Atomic.fetch_and_add t.tick 1 in
    Logs.debug (fun f ->
        f "[scheduler=%d,tick=%d] Adding task: %a" t.uid tick pp_task task);
    Task_queue.add t.tasks (tick, task)

  let spawn_process scheduler fn =
    let proc = Process.make () in
    Process_table.register_process scheduler.processes proc;
    let state = Proc_state.make fn Yield in
    add_task scheduler (Suspended (proc, state));
    proc

  let send_message scheduler pid msg =
    match Process_table.get scheduler.processes pid with
    | Some proc ->
        Logs.info (fun f -> f "delivering message to %a" Pid.pp pid);
        Mailbox.queue proc.mailbox msg;
        add_task scheduler (Signaled (pid, Message))
    | None ->
        Logs.info (fun f -> f "COULD NOT DELIVER message to %a" Pid.pp pid)

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
          send_message scheduler pid msg;
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
      | Spawn fn ->
          let proc = spawn_process scheduler fn in
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
    | Arrived (_proc, _fn) -> ()
    | Terminated _ -> ()
    | Signaled (pid, signal) -> (
        match (Process_table.get scheduler.processes pid, signal) with
        | Some proc, Message -> Process.set_pending proc
        | _ -> ())
    | Deliver_message (_pid, _msg) -> ()
    | Suspended (proc, proc_state) ->
        let state =
          match Process.state proc with
          | Exited _ | Waiting_receive -> proc_state
          | Running | Pending ->
              let perform = perform pool scheduler (Process.pack proc) in
              Proc_state.run ~perform proc_state
        in
        handle pool scheduler proc state

  let is_idle t = Task_queue.is_empty t.tasks

  let run pool scheduler () =
    Logs.debug (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         Mutex.lock pool.mutex;
         while is_idle scheduler && not pool.stop do
           Condition.wait pool.condition_pending_work pool.mutex
         done;
         Mutex.unlock pool.mutex;

         if pool.stop then raise_notrace Exit;

         match Task_queue.next scheduler.tasks with
         | None ->
             Logs.debug (fun f -> f "no tasks");
             ()
         | Some (_tick, task) ->
             Logs.debug (fun f -> f "found task: %a" pp_task task);
             once pool scheduler task;
             ()
       done
     with Exit -> Mutex.unlock pool.mutex);
    Logs.debug (fun f -> f "< exit worker loop")

  let self () = Effect.perform Scheduler_self
  let count () = Effect.perform Scheduler_count
  let list () = Effect.perform Schedulers
end

(** handles spinning up several schedulers and synchronizing the shutdown *)
module Pool = struct
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
      Stdlib.Domain.spawn (fun () -> Scheduler.run pool scheduler ())
    in
    Logs.debug (fun f -> f "Created %d schedulers" (List.length schedulers));
    (pool, List.map spawn schedulers)
end

(** Public API *)

let self () = Effect.perform Self
let send pid msg = Effect.perform (Send (msg, pid))
let yield () = Effect.perform Yield

let spawn fn =
  Effect.perform
    (Spawn
       (fun () ->
         fn ();
         Normal))

let receive ?(select = fun _ -> Message.Take) () =
  Effect.perform (Receive { select })

let run ?(rnd = Random.State.make_self_init ()) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Scheduler.Uid.reset ();
  let sch0 = Scheduler.make ~rnd () in
  let _pid =
    Scheduler.spawn_process sch0 (fun () ->
        main ();
        Normal)
  in
  let pool, domains = Pool.make ~main:sch0 () in
  Scheduler.run pool sch0 ();
  Logs.debug (fun f -> f "Riot runtime shutting down...");
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  Logs.debug (fun f -> f "Riot runtime shutdown");
  ()
