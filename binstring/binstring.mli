type t

val empty : t
val length : t -> int

exception Malformed of string

val of_string : string -> t
val to_string : t -> string

exception View_out_of_bounds

val sub : ?off:int -> len:int -> t -> t
val join : t -> t -> t
val ( ^ ) : t -> t -> t
val concat : t -> t list -> t

module Extract : sig
  exception Invalid_position
  exception Byte_not_found

  val get_bits : t -> int -> char
  val get_byte : t -> int -> char
end

