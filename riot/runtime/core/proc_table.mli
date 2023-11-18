exception Reregistering_process of Process.t

type t

val create : unit -> t
val get : t -> Pid.t -> Process.t option
val register_process : t -> Process.t -> unit
val processes : t -> (Pid.t * Process.t) Seq.t
