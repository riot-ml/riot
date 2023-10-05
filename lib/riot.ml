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

type state = 
  | Runnable | Waiting | Running | Exited of exn | Suspended

type process = {
  pid : Pid.t;
  state : state Atomic.t;
  cont : exit_reason Proc_state.t Atomic.t;
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
  | Link : Pid.t -> unit Effect.t
  | Monitor : Pid.t -> unit Effect.t
  | Random : Random.State.t Effect.t
  | Receive : {
      select : Message.t -> Message.select_marker;
    }
      -> Message.t Effect.t
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
  | Self -> Format.fprintf ppf "Self"
  | Send _ -> Format.fprintf ppf "Send"
  | Signal _ -> Format.fprintf ppf "Signal"
  | Spawn _ -> Format.fprintf ppf "Spawn"
  | Yield -> Format.fprintf ppf "Yield"
  | _effect -> Format.fprintf ppf "Unhandled effect"


let pp_signal ppf (t: signal) =
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

  let cont t = Atomic.get t.cont
  let set_cont c t = Atomic.set t.cont c
  let state t = Atomic.get t.state

  let make fn =
    let cont = Proc_state.make fn Yield in
    let pid = Pid.next () in
    Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
    {
      pid;
      cont = Atomic.make cont;
      state = Atomic.make Runnable;
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

(** Scheduler *)

type scheduler = {
  uid : Scheduler_uid.t;
  rnd : Random.State.t;
  ready_queue : process Queue.t;
  processes : Process_table.t;
}

type pool = {
  mutable stop : bool;
  condition_pending_work : Condition.t;
  mutex : Mutex.t;
  schedulers : scheduler list;
}

let pp_process ppf t = Format.fprintf ppf "pid=%a" Pid.pp t.pid

module Scheduler = struct
  module Uid = Scheduler_uid

  let make ~rnd () =
    let uid = Uid.next () in
    Logs.debug (fun f -> f "Making scheduler with id: %d" uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      ready_queue = Queue.create ();
      processes = Process_table.create ();
    }

  let spawn_process scheduler fn =
    let proc = Process.make fn in
    Process_table.register_process scheduler.processes proc;
    Queue.add proc scheduler.ready_queue;
    proc

  let send_message scheduler pid msg =
    match Process_table.get scheduler.processes pid with
    | Some proc ->
        Logs.info (fun f -> f "delivering message to %a" Pid.pp pid);
        Mailbox.queue proc.mailbox msg
    | None ->
        Logs.info (fun f -> f "COULD NOT DELIVER message to %a" Pid.pp pid)

  let perform scheduler process =
    let open Proc_state in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.debug (fun f -> f "performing effect: %a" pp_effect eff);
      match eff with
      | Random -> k (Continue scheduler.rnd)
      | Yield -> k Yield
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

  let once scheduler proc =
    match Process.state proc with
    | Suspended | Exited _ | Waiting -> ()
    | Running | Runnable -> (
        let perform = perform scheduler proc in
        let cont = Proc_state.run ~perform (Process.cont proc) in
        Process.set_cont cont proc;
        match cont with
        | Proc_state.Finished _reason -> ()
        | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
            Queue.add proc scheduler.ready_queue)

  let is_idle t = Queue.is_empty t.ready_queue

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

         match Queue.take_opt scheduler.ready_queue with
         | None ->
             Logs.debug (fun f -> f "no ready processes");
             ()
         | Some proc ->
             Logs.debug (fun f -> f "found process: %a" pp_process proc);
             once scheduler proc;
             ()
       done
     with Exit -> Mutex.unlock pool.mutex);
    Logs.debug (fun f -> f "< exit worker loop")
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
        stop = false;
        schedulers = [ main ] @ schedulers;
        condition_pending_work = Condition.create ();
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
