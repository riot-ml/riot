module Proc_state = Proc_state
module Logs = Logs

(** Uid Modules *)

module Scheduler_uid = struct
  type t = int

  let __current__ = Atomic.make 0
  let next () = Atomic.fetch_and_add __current__ 1
  let equal = Int.equal

  let reset () =
    Logs.debug (fun f -> f "Resetting Scheduler Uids");
    Atomic.set __current__ 0
end

module Pid = struct
  type t = int

  let zero : t = 0
  let __current__ = Atomic.make 0
  let next () = Atomic.fetch_and_add __current__ 1
  let pp ppf pid = Format.fprintf ppf "<0.%d.0>" pid

  let reset () =
    Logs.debug (fun f -> f "Resetting Process Ids");
    Atomic.set __current__ 0
end

module Message = struct
  type select_marker = Take | Skip
  type t = ..
  type t += Exit_signal
end

module Mailbox = struct
  type t = {
    size: int Atomic.t;
    queue: Message.t Queue.t;
  }

  let create () = { size = Atomic.make 0; queue = Queue.create () }

  let queue t msg =
    Atomic.incr t.size;
    Queue.add msg t.queue

  let next (t : t) = 
    Atomic.decr t.size;
    Queue.take_opt t.queue

  let is_empty (t : t) = Queue.is_empty t.queue

  let merge (a : t) (b : t) = Queue.merge a.queue b.queue

  let size (t : t) = Atomic.get t.size
end

(** Process *)

exception Process_killed

type state =
  | Runnable
  | Waiting
  | Running
  | Exited of (exit_reason, exn) result
  | Suspended

and process = {
  pid : Pid.t;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  links : Pid.t list;
  monitors : Pid.t list;
}
(** ['msg process] an internal process descriptor. Represents a process in the runtime. *)

(** [exit_reason] indicates why a process was terminated. *)
and exit_reason = Normal | Exit_signal | Timeout_value | Bad_link

(** [signal]s are used to communicate to a process that a given action needs to be performed by it. They are low-level primitives used by the runtime. *)
and signal = Link | Unlink | Exit | Monitor | Demonitor | Message

(** Effects *)

[@@@warning "-30"]

type _ Effect.t +=
  | Receive : {
      select : Message.t -> Message.select_marker;
    }
      -> Message.t Effect.t
  | Yield : unit Effect.t

let pp_effect : type a. Format.formatter -> a Effect.t -> unit =
 fun ppf eff ->
  match eff with
  | Receive _ -> Format.fprintf ppf "Receive"
  | Yield -> Format.fprintf ppf "Yield"
  | _effect -> Format.fprintf ppf "Unhandled effect"

let pp_signal ppf (t : signal) =
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

  let cont t = t.cont
  let set_cont c t = t.cont <- c
  let state t = Atomic.get t.state

  let is_alive t =
    match Atomic.get t.state with
    | Runnable | Waiting | Running | Suspended -> true
    | Exited _ -> false

  let mark_as_dead t reason = Atomic.set t.state (Exited reason)
  let mark_as_awaiting_message t = Atomic.set t.state Waiting
  let mark_as_runnable t = Atomic.set t.state Runnable

  let make fn =
    let cont = Proc_state.make fn Yield in
    let pid = Pid.next () in
    Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
    {
      pid;
      cont;
      state = Atomic.make Runnable;
      links = [];
      monitors = [];
      mailbox = Mailbox.create ();
    }

  let pid { pid; _ } = pid
end

module Process_table = struct
  type t = { processes : (Pid.t, Process.t) Hashtbl.t; lock : Mutex.t }

  let create () = { lock = Mutex.create (); processes = Hashtbl.create 16_000 }
  let register_process t proc =
    Mutex.lock t.lock;
    Hashtbl.add t.processes proc.pid proc;
    Mutex.unlock t.lock

  let get t pid = Hashtbl.find_opt t.processes pid
  let remove t pid = Hashtbl.remove t.processes pid
  let process_count t = Hashtbl.length t.processes

  let processes t = Hashtbl.to_seq t.processes
end

(** Scheduler *)

type scheduler = {
  uid : Scheduler_uid.t;
  rnd : Random.State.t;
  ready_queue : process Queue.t;
}

type pool = {
  mutable stop : bool;
  condition_pending_work : Condition.t;
  mutex : Mutex.t;
  schedulers : scheduler list;
  processes : Process_table.t;
}

let pp_process ppf t = Format.fprintf ppf "pid=%a" Pid.pp t.pid

module Scheduler = struct
  module Uid = Scheduler_uid

  let make ~rnd () =
    let uid = Uid.next () in
    Logs.debug (fun f -> f "Making scheduler with id: %d" uid);
    { uid; rnd = Random.State.copy rnd; ready_queue = Queue.create () }

  let current_scheduler : scheduler Domain.DLS.key option ref = ref None

  let get_current_scheduler : unit -> scheduler =
   fun () -> !current_scheduler |> Option.get |> Domain.DLS.get

  let set_current_scheduler : scheduler -> unit =
   fun scheduler ->
    current_scheduler := Some (Domain.DLS.new_key (fun () -> scheduler))

  let current_process_pid : Pid.t Domain.DLS.key ref =
    ref (Domain.DLS.new_key (fun () -> Pid.zero))

  let get_current_process_pid : unit -> Pid.t =
   fun () -> !current_process_pid |> Domain.DLS.get

  let set_current_process_pid : Pid.t -> unit =
   fun process_pid ->
    current_process_pid := Domain.DLS.new_key (fun () -> process_pid)

  let get_random_scheduler : pool -> scheduler =
   fun pool ->
    let scheduler = get_current_scheduler () in
    let all_schedulers = pool.schedulers in
    let rnd_idx = Random.State.int scheduler.rnd (List.length all_schedulers) in
    List.nth all_schedulers rnd_idx

  let perform _scheduler process =
    let open Proc_state in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.debug (fun f -> f "performing effect: %a" pp_effect eff);
      match eff with
      | Yield -> k Yield
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
          if Mailbox.is_empty process.mailbox then (
            Logs.debug (fun f ->
              f "%a is awaiting for new messages" Pid.pp process.pid);
            Process.mark_as_awaiting_message process;
            k (Delay effect))
          else
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
      | effect -> k (Reperform effect)
    in
    { perform }

  let step_process scheduler proc =
    set_current_process_pid proc.pid;
    match Process.state proc with
    | Suspended | Waiting -> Queue.add proc scheduler.ready_queue
    | Exited _ -> ()
    | Running | Runnable -> (
        let perform = perform scheduler proc in
        let cont = Proc_state.run ~perform (Process.cont proc) in
        Process.set_cont cont proc;
        match cont with
        | Proc_state.Finished reason -> Process.mark_as_dead proc reason
        | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
            Queue.add proc scheduler.ready_queue)

  let is_idle t = Queue.is_empty t.ready_queue

  let run pool scheduler () =
    Logs.debug (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         Domain.cpu_relax ();
         if pool.stop then raise_notrace Exit;
         match Queue.take_opt scheduler.ready_queue with
         | None ->
             Logs.debug (fun f -> f "no ready processes");
             ()
         | Some proc ->
             Logs.info (fun f -> f "found process: %a" pp_process proc);
             step_process scheduler proc;
             ()
       done
     with Exit -> ());
    Logs.debug (fun f -> f "< exit worker loop")
end

(** handles spinning up several schedulers and synchronizing the shutdown *)
module Pool = struct
  let current_pool : pool Domain.DLS.key option ref = ref None

  let get_pool : unit -> pool =
   fun () -> !current_pool |> Option.get |> Domain.DLS.get

  let set_pool : pool -> unit =
   fun pool -> current_pool := Some (Domain.DLS.new_key (fun () -> pool))

  let shutdown pool = pool.stop <- true

  let make ?(rnd = Random.State.make_self_init ()) ~domains ~main () =
    Logs.debug (fun f -> f "Making scheduler pool...");
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in
    let pool =
      {
        mutex = Mutex.create ();
        stop = false;
        schedulers = [ main ] @ schedulers;
        condition_pending_work = Condition.create ();
        processes = Process_table.create ();
      }
    in
    let spawn scheduler =
      Stdlib.Domain.spawn (fun () -> Scheduler.run pool scheduler ())
    in
    Logs.debug (fun f -> f "Created %d schedulers" (List.length schedulers));
    (pool, List.map spawn schedulers)
end

(** Public API *)

let yield () = Effect.perform Yield
let self () = Scheduler.get_current_process_pid ()

(* NOTE(leostera): to send a message, we will find the receiver process
   in the process table and queue at the back of their mailbox
*)
let send pid msg =
  let pool = Pool.get_pool () in
  match Process_table.get pool.processes pid with
  | Some proc ->
      Logs.debug (fun f -> f "delivering message meant for %a to %a" Pid.pp pid Pid.pp proc.pid);
      Mailbox.queue proc.mailbox msg;
      Process.mark_as_runnable proc
  | None ->
      (* Effect.perform (Send (msg, pid)) *)
      Logs.debug (fun f -> f "COULD NOT DELIVER message to %a" Pid.pp pid)

let _spawn pool scheduler fn =
  let proc =
    Process.make (fun () ->
        fn ();
        Normal)
  in
  Process_table.register_process pool.processes proc;
  Queue.add proc scheduler.ready_queue;
  proc.pid

let spawn fn =
  let pool = Pool.get_pool () in
  let scheduler = Scheduler.get_random_scheduler pool in
  _spawn pool scheduler fn

let processes () =
  yield ();
  let pool = Pool.get_pool () in
  Process_table.processes pool.processes

let is_process_alive pid =
  yield ();
  let pool = Pool.get_pool () in
  match Process_table.get pool.processes pid with
  | Some proc -> 
      Process.is_alive proc
  | None -> false

let random () = (Scheduler.get_current_scheduler ()).rnd

let receive ?(select = fun _ -> Message.Take) () =
  Effect.perform (Receive { select })

let shutdown () =
  let pool = Pool.get_pool () in
  Pool.shutdown pool

let run ?(rnd = Random.State.make_self_init ())
    ?(workers = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Process.Pid.reset ();
  Scheduler.Uid.reset ();
  let sch0 = Scheduler.make ~rnd () in
  Scheduler.set_current_scheduler sch0;
  let pool, domains = Pool.make ~main:sch0 ~domains:workers () in
  Pool.set_pool pool;
  let _pid = _spawn pool sch0 main in
  Scheduler.run pool sch0 ();
  Logs.debug (fun f -> f "Riot runtime shutting down...");
  List.iter Stdlib.Domain.join domains;
  Logs.debug (fun f -> f "Riot runtime shutdown");
  ()
