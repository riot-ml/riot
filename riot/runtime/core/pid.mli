type t

val zero : t
val equal : t -> t -> bool
val next : unit -> t
val pp : Format.formatter -> t -> unit
val reset : unit -> unit
val compare : t -> t -> int
val hash : t -> int

module Map : Util.Dashmap.Intf with type key = t
