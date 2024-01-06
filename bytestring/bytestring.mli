type t
(** an immutable efficient binary string *)

val empty : t
val length : t -> int

exception No_match
exception Guard_mismatch
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
  val next_bits : size:int -> t -> int
  val next_byte : t -> string
  val next_bytes : size:int -> t -> string
  val next_utf8 : t -> string
  val next_utf8_seq : len:int -> t -> string
  val rest : t -> string
  val expect_empty : t -> unit
  val expect_bits : t -> int -> unit
  val expect_bytes : t -> string -> unit
  val expect_literal_int : t -> ?size:int -> int -> unit
  val expect_literal_string : t -> ?size:string -> Stdlib.String.t -> unit
end

val to_iter : t -> Iter.t

module Transient : sig
  type string = t
  type t

  val create : unit -> t
  val add_string : t -> ?size:int -> string -> unit
  val add_bits : t -> ?size:int -> int -> unit
  val add_utf8 : t -> ?size:int -> string -> unit
  val add_literal_int : t -> ?size:int -> int -> unit
  val add_literal_utf8 : t -> ?size:int -> Stdlib.String.t -> unit
  val add_literal_string : t -> ?size:int -> Stdlib.String.t -> unit
  val commit : t -> string
end

val to_transient : t -> Transient.t
