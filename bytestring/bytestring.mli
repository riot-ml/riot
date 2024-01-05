type t
(** an immutable efficient binary string *)

val empty : t
val length : t -> int

exception Malformed of string

val of_string : string -> t
val to_string : t -> string

exception View_out_of_bounds

val view : ?off:int -> len:int -> t -> t
val join : t -> t -> t
val ( ^ ) : t -> t -> t
val concat : t -> t list -> t

module Iter : sig
  type string = t
  type t

  exception Invalid_position
  exception Byte_not_found

  val next_bit : t -> int
  val next_bits : size:int -> t -> int64
  val next_byte : t -> string
  val next_bytes : size:int -> t -> string
  val next_utf8 : t -> string
  val next_utf8_seq : len:int -> t -> string
  val rest : t -> string

  val expect_bits : int -> t -> unit
  val expect_bytes : string -> t -> unit
end

val to_iter : t -> Iter.t

module Transient : sig
  type string = t
  type t

  val add_string : string -> t -> t
  val commit : t -> string
end

val to_transient : t -> Transient.t
