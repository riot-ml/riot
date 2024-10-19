type t

val create : unit -> t
val remove : t -> Process.t -> unit
val contains : t -> Process.t -> bool
val size : t -> int
val add : t -> Process.t -> unit
