type t

val create : unit -> t
val queue : t -> Message.envelope -> unit
val next : t -> Message.envelope option
val size : t -> int
val is_empty : t -> bool
