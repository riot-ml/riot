type t

val create : unit -> t
val size : t -> int
val queue : t -> Process.t -> unit
val next : t -> Process.t option
