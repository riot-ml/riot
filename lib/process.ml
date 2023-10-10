type exit_reason =
  | Normal
  | Exit_signal
  | Timeout_value
  | Bad_link
  | Exception of exn

let pp_reason ppf (t : exit_reason) =
  match t with
  | Normal -> Format.fprintf ppf "Normal"
  | Exit_signal -> Format.fprintf ppf "Exit_signal"
  | Timeout_value -> Format.fprintf ppf "Timeout"
  | Bad_link -> Format.fprintf ppf "Bad_link"
  | Exception exn -> Format.fprintf ppf "Exception: %s" (Printexc.to_string exn)

module Messages = struct
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason
end

type state = Runnable | Waiting | Running | Exited of exit_reason
type process_flags = { mutable trap_exits : bool }
type process_flag = Trap_exit of bool

type t = {
  pid : Pid.t;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  links : Pid.t list Atomic.t;
  monitors : Pid.t list Atomic.t;
  flags : process_flags;
}

let pp ppf t = Format.fprintf ppf "pid=%a" Pid.pp t.pid
let cont t = t.cont
let set_cont c t = t.cont <- c
let state t = Atomic.get t.state
let set_flag t flag = match flag with Trap_exit v -> t.flags.trap_exits <- v

let is_alive t =
  match Atomic.get t.state with
  | Runnable | Waiting | Running -> true
  | Exited _ -> false

let mark_as_running t = Atomic.set t.state Running
let mark_as_dead t reason = Atomic.set t.state (Exited reason)

let mark_as_awaiting_message t =
  if Mailbox.is_empty t.mailbox then Atomic.set t.state Waiting

let mark_as_runnable t = Atomic.set t.state Runnable
let add_link t (pid : Pid.t) = Atomic.set t.links (pid :: Atomic.get t.links)
let pid { pid; _ } = pid
let default_flags = { trap_exits = false }

let make fn =
  let cont = Proc_state.make fn Proc_effect.Yield in
  let pid = Pid.next () in
  Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
  {
    pid;
    cont;
    state = Atomic.make Runnable;
    links = Atomic.make [];
    monitors = Atomic.make [];
    mailbox = Mailbox.create ();
    flags = default_flags;
  }
