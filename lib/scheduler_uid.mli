type t

val equal : t -> t -> bool
val next : unit -> t
val pp : Format.formatter -> t -> unit
val reset : unit -> unit
