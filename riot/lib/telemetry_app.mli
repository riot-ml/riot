open Runtime

val name : string
val start : unit -> (Pid.t, 'a) result

type event = Telemetry.event = ..

val emit : Telemetry.event -> unit
val attach : (Telemetry.event -> unit) -> unit
