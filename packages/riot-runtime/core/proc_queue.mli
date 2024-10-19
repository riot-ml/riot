type t

val create : unit -> t
val size : t -> int
val is_empty : t -> bool
val queue : t -> Process.t -> unit
val next : t -> Process.t option
val remove : t -> Process.t -> unit
