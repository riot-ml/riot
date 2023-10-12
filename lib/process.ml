type exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

type signal = Message | Exit of exit_reason | Link of Pid.t | Monitor of Pid.t

module Messages = struct
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason
end

type state = Runnable | Waiting | Running | Exited of exit_reason
type process_flags = { mutable trap_exits : bool }
type process_flag = Trap_exit of bool

let default_flags = { trap_exits = false }

type t = {
  lock : Mutex.t;
  pid : Pid.t;
  sid : Scheduler_uid.t;
  flags : process_flags;
  mutable state : state;
  mutable links : Pid.t list;
  mutable monitors : Pid.t list;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  signals : signal Lf_queue.t;
  ext_mailbox : Mailbox.t;  (** external messages always go here first *)
  ext_signals : signal Lf_queue.t;  (** all external signals are added here *)
}
(** The process descriptor. *)

type t_locked = { locked : t } [@@unboxed]

let make sid fn =
  let cont = Proc_state.make fn Proc_effect.Yield in
  let pid = Pid.next () in
  Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
  let proc =
    {
      lock = Mutex.create ();
      pid;
      sid;
      cont;
      state = Runnable;
      links = [];
      monitors = [];
      mailbox = Mailbox.create ();
      flags = default_flags;
      signals = Lf_queue.create ();
      ext_mailbox = Mailbox.create ();
      ext_signals = Lf_queue.create ();
    }
  in
  proc

let lock t =
  Mutex.lock t.lock;
  { locked = t }

let unlock { locked } = Mutex.unlock locked.lock
let cont { locked = t } = t.cont
let pid { locked = { pid; _ } } = pid
let sid { locked = { sid; _ } } = sid
let state { locked = t } = t.state
let signals { locked = t } = t.signals
let monitors { locked = t } = t.monitors
let links { locked = t } = t.links

let is_alive t =
  match t.state with Runnable | Waiting | Running -> true | Exited _ -> false

let is_exited t = match t.state with Exited _ -> true | _ -> false
let is_runnable { locked = t } = t.state = Runnable
let is_running { locked = t } = t.state = Running
let is_waiting { locked = t } = t.state = Waiting
let has_empty_mailbox { locked = t } = Mailbox.is_empty t.mailbox
let has_messages { locked = t } = not (Mailbox.is_empty t.mailbox)
let should_awake t = is_alive t.locked && has_messages t
let mark_as_awaiting_message { locked = t } = t.state <- Waiting
let mark_as_running { locked = t } = t.state <- Running
let mark_as_dead { locked = t } reason = t.state <- Exited reason
let mark_as_runnable { locked = t } = t.state <- Runnable

(** `set_flag` is only called by `Riot.process_flag` which runs only on the
    current process, which means we already have a lock on it.
 *)
let set_flag t flag = match flag with Trap_exit v -> t.flags.trap_exits <- v

let set_cont { locked = t } c = t.cont <- c

(** pop the next message, and if we're out of inner messages, move the external
    messages into the inner mailbox.
  *)
let rec next_message { locked = t } =
  match Mailbox.next t.mailbox with
  | Some m -> Some m
  | None when not (Mailbox.is_empty t.ext_mailbox) ->
      Mailbox.merge t.mailbox t.ext_mailbox;
      next_message { locked = t }
  | None -> None

let append_mailbox { locked = t } mailbox = Mailbox.merge t.mailbox mailbox
let add_signal t (signal : signal) = Lf_queue.add signal t.ext_signals

let send_message t msg =
  Mailbox.queue t.ext_mailbox msg;
  add_signal t Message

let rec pp ppf t =
  Format.fprintf ppf "Process%a { state = %a; messages = %d }" Pid.pp t.pid
    pp_state t.state (Mailbox.size t.mailbox)

and pp_state ppf (state : state) =
  match state with
  | Runnable -> Format.fprintf ppf "Runnable"
  | Waiting -> Format.fprintf ppf "Waiting"
  | Running -> Format.fprintf ppf "Running"
  | Exited e -> Format.fprintf ppf "Exited(%a)" pp_reason e

and pp_reason ppf (t : exit_reason) =
  match t with
  | Normal -> Format.fprintf ppf "Normal"
  | Link_down pid -> Format.fprintf ppf "Link_down(%a)" Pid.pp pid
  | Exit_signal -> Format.fprintf ppf "Exit_signal"
  | Bad_link -> Format.fprintf ppf "Bad_link"
  | Exception exn -> Format.fprintf ppf "Exception: %s" (Printexc.to_string exn)
