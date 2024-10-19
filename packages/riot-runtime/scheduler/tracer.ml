open Core

let tracer_send : (Pid.t -> Process.t -> Message.t -> unit) ref =
  ref (fun _sender _proc _msg -> ())

let trace_send fn = tracer_send := fn

let tracer_proc_run : (int -> Process.t -> unit) ref =
  ref (fun _sch _proc -> ())

let trace_proc_run fn = tracer_proc_run := fn
