module Mode : sig
  type t = [ `r | `rw | `w ]

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type fd = Unix.file_descr
type t

val get : t -> fd option
val to_int : t -> int
val make : fd -> t
val is_open : t -> bool
val is_closed : t -> bool
val close : t -> unit
val use : op_name:string -> t -> (fd -> 'a) -> 'a
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
