val spawn : (unit -> unit) -> Core.Pid.t
val monitor : Core.Pid.t -> Core.Pid.t -> unit
val link : Core.Pid.t -> unit
val exit : Core.Pid.t -> Core.Process.exit_reason -> unit
val flag : Core.Process.process_flag -> unit
val register : string -> Core.Pid.t -> unit
val unregister : string -> unit

type exit_reason = Core.Process.exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Core.Pid.t
  | Exception of exn

module Messages = Core.Process.Messages

type state = Core.Process.state =
  | Runnable
  | Waiting_message
  | Waiting_io of { syscall : string; mode : [ `r | `rw | `w ]; fd : Util.Fd.t }
  | Running
  | Exited of exit_reason
  | Finalized

type process_flags = Core.Process.process_flags = { trap_exits : bool Atomic.t }
type process_flag = Core.Process.process_flag = Trap_exit of bool

val default_flags : unit -> process_flags

type t = Core.Process.t = {
  pid : Core.Pid.t;
  sid : Core.Scheduler_uid.t;
  flags : process_flags;
  state : state Atomic.t;
  mutable cont : exit_reason Core.Proc_state.t;
  mailbox : Core.Mailbox.t;
  save_queue : Core.Mailbox.t;
  mutable read_save_queue : bool;
  links : Core.Pid.t list Atomic.t;
  monitors : Core.Pid.t list Atomic.t;
}

exception Process_reviving_is_forbidden of t

val make : Core.Scheduler_uid.t -> (unit -> exit_reason) -> t
val pp : Format.formatter -> t -> unit
val pp_state : Format.formatter -> state -> unit
val pp_reason : Format.formatter -> exit_reason -> unit
val pp_flags : Format.formatter -> process_flags -> unit
val cont : t -> exit_reason Core.Proc_state.t
val pid : t -> Core.Pid.t
val sid : t -> Core.Scheduler_uid.t
val state : t -> state
val monitors : t -> Core.Pid.t list
val links : t -> Core.Pid.t list
val is_alive : t -> bool
val is_exited : t -> bool
val is_waiting : t -> bool
val is_waiting_io : t -> bool
val is_runnable : t -> bool
val is_running : t -> bool
val is_finalized : t -> bool
val has_empty_mailbox : t -> bool
val has_messages : t -> bool
val message_count : t -> int
val should_awake : t -> bool
val mark_as_awaiting_io : t -> string -> [ `r | `rw | `w ] -> Util.Fd.t -> unit
val mark_as_awaiting_message : t -> unit
val mark_as_running : t -> unit
val mark_as_runnable : t -> unit
val mark_as_exited : t -> exit_reason -> unit
val mark_as_finalized : t -> unit
val set_flag : t -> process_flag -> unit
val set_cont : t -> exit_reason Core.Proc_state.t -> unit
val add_link : t -> Core.Pid.t -> unit
val add_monitor : t -> Core.Pid.t -> unit
val next_message : t -> Core.Message.envelope option
val add_to_save_queue : t -> Core.Message.envelope -> unit
val read_save_queue : t -> unit
val send_message : t -> Core.Message.t -> unit
