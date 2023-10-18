type fd = Unix.file_descr
type t

val get : t -> fd option
val to_int : t -> int
val make : release:(t -> unit) -> fd -> t
val close : t -> unit
val use : op_name:string -> t -> (fd -> 'a) -> 'a
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
