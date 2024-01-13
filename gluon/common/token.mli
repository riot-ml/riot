type t

val equal : t -> t -> bool
val next : unit -> t
val to_int : t -> int
val of_int : int -> t
val pp : Format.formatter -> t -> unit
