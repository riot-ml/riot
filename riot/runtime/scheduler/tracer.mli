open Core

val tracer_send : (Pid.t -> Process.t -> Message.t -> unit) ref
val trace_send : (Pid.t -> Process.t -> Message.t -> unit) -> unit
val tracer_proc_run : (int -> Process.t -> unit) ref
val trace_proc_run : (int -> Process.t -> unit) -> unit
