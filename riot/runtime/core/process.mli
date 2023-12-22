open Util

type exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

module Messages : sig
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason
end

type state =
  | Runnable
  | Waiting_message
  | Waiting_io of { syscall : string; mode : [ `r | `rw | `w ]; fd : Fd.t }
  | Running
  | Exited of exit_reason
  | Finalized

type process_flags = { trap_exits : bool Atomic.t }
type process_flag = Trap_exit of bool

val default_flags : unit -> process_flags

type t = {
  pid : Pid.t;
  sid : Scheduler_uid.t;
  flags : process_flags;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable read_save_queue : bool;
  links : Pid.t list Atomic.t;
  monitors : Pid.t list Atomic.t;
  ready_fds : Fd.t list Atomic.t;
}

exception Process_reviving_is_forbidden of t

val make : Scheduler_uid.t -> (unit -> exit_reason) -> t
val pp : Format.formatter -> t -> unit
val pp_state : Format.formatter -> state -> unit
val pp_reason : Format.formatter -> exit_reason -> unit
val pp_flags : Format.formatter -> process_flags -> unit
val cont : t -> exit_reason Proc_state.t
val pid : t -> Pid.t
val sid : t -> Scheduler_uid.t
val state : t -> state
val monitors : t -> Pid.t list
val links : t -> Pid.t list
val is_alive : t -> bool
val is_exited : t -> bool
val is_waiting : t -> bool
val is_waiting_io : t -> bool
val is_runnable : t -> bool
val is_running : t -> bool
val is_finalized : t -> bool
val has_empty_mailbox : t -> bool
val has_messages : t -> bool
val has_ready_fds : t -> bool
val set_ready_fds : t -> Fd.t list -> unit
val message_count : t -> int
val should_awake : t -> bool
val mark_as_awaiting_io : t -> string -> [ `r | `rw | `w ] -> Fd.t -> unit
val mark_as_awaiting_message : t -> unit
val mark_as_running : t -> unit
val mark_as_runnable : t -> unit
val mark_as_exited : t -> exit_reason -> unit
val mark_as_finalized : t -> unit
val set_flag : t -> process_flag -> unit
val set_cont : t -> exit_reason Proc_state.t -> unit
val add_link : t -> Pid.t -> unit
val add_monitor : t -> Pid.t -> unit
val next_message : t -> Message.envelope option
val add_to_save_queue : t -> Message.envelope -> unit
val read_save_queue : t -> unit
val send_message : t -> Message.t -> unit
