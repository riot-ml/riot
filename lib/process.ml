type exit_reason = Normal | Exit_signal | Bad_link | Exception of exn
type signal = Message

module Messages = struct
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason
end

type state = Runnable | Waiting | Running | Exited of exit_reason
type process_flags = { mutable trap_exits : bool }
type process_flag = Trap_exit of bool

let default_flags = { trap_exits = false }

type t = {
  pid : Pid.t;
  sid : Scheduler_uid.t;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  links : Pid.t list Atomic.t;
  monitors : Pid.t list Atomic.t;
  flags : process_flags;
  mutable signals : signal list Atomic.t;
}

let make sid fn =
  let cont = Proc_state.make fn Proc_effect.Yield in
  let pid = Pid.next () in
  Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
  let proc =
    {
      pid;
      sid;
      cont;
      state = Atomic.make Runnable;
      links = Atomic.make [];
      monitors = Atomic.make [];
      mailbox = Mailbox.create ();
      flags = default_flags;
      signals = Atomic.make [];
    }
  in
  proc

let cont t = t.cont
let pid { pid; _ } = pid
let state t = Atomic.get t.state
let signals t = Atomic.get t.signals
let has_empty_mailbox t = Mailbox.is_empty t.mailbox
let has_messages t = not (Mailbox.is_empty t.mailbox)

let is_alive t =
  match Atomic.get t.state with
  | Runnable | Waiting | Running -> true
  | Exited _ -> false

let is_waiting t = Atomic.get t.state = Waiting

let mark_as_awaiting_message t =
  if Mailbox.is_empty t.mailbox then Atomic.set t.state Waiting

let mark_as_running t = Atomic.set t.state Running
let mark_as_dead t reason = Atomic.set t.state (Exited reason)
let mark_as_runnable t = Atomic.set t.state Runnable
let set_flag t flag = match flag with Trap_exit v -> t.flags.trap_exits <- v
let set_cont c t = t.cont <- c
let add_link t (pid : Pid.t) = Atomic.set t.links (pid :: Atomic.get t.links)

let add_signal t (signal : signal) =
  Atomic.set t.signals (signal :: Atomic.get t.signals)

let send_message t msg =
  Mailbox.queue t.mailbox msg;
  mark_as_runnable t

let rec pp ppf t =
  Format.fprintf ppf "Process%a { state = %a; messages = %d }" Pid.pp t.pid
    pp_state (Atomic.get t.state) (Mailbox.size t.mailbox)

and pp_state ppf (state : state) =
  match state with
  | Runnable -> Format.fprintf ppf "Runnable"
  | Waiting -> Format.fprintf ppf "Waiting"
  | Running -> Format.fprintf ppf "Running"
  | Exited e -> Format.fprintf ppf "Exited(%a)" pp_reason e

and pp_reason ppf (t : exit_reason) =
  match t with
  | Normal -> Format.fprintf ppf "Normal"
  | Exit_signal -> Format.fprintf ppf "Exit_signal"
  | Bad_link -> Format.fprintf ppf "Bad_link"
  | Exception exn -> Format.fprintf ppf "Exception: %s" (Printexc.to_string exn)
