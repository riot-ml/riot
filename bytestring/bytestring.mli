type t
(** an immutable efficient binary string *)

type view = { offset : int; length : int; data : string }
(** A valid sub-range with an associated string.

      When used inside representations, it should always be a, non-empty, and a
      strict sub-range of the associated string.
   *)

val empty : t
val is_empty : t -> bool
val length : t -> int
val pp : Format.formatter -> t -> unit

exception No_match
exception Guard_mismatch
exception Malformed of string

val of_string : string -> t
val to_string : t -> string
val to_iovec : t -> Rio.Iovec.t

exception View_out_of_bounds

val join : t -> t -> t
val ( ^ ) : t -> t -> t
val concat : t -> t list -> t
val sub : ?off:int -> len:int -> t -> t

module Iter : sig
  type bytestring = t
  type t

  exception Invalid_position
  exception Byte_not_found

  val next_bit : t -> int
  val next_bits : size:int -> t -> int
  val next_byte : t -> bytestring
  val next_bytes : size:int -> t -> bytestring
  val next_utf8 : t -> bytestring
  val next_utf8_seq : len:int -> t -> bytestring
  val rest : t -> bytestring
  val rest_as_string : t -> string
  val expect_empty : t -> unit
  val expect_bits : t -> int -> unit
  val expect_bytes : t -> bytestring -> unit
  val expect_literal_int : t -> ?size:int -> int -> unit
  val expect_literal_string : t -> ?size:int -> Stdlib.String.t -> unit
end

val to_iter : t -> Iter.t

module Transient : sig
  type bytestring = t
  type t

  val create : unit -> t
  val add_string : t -> ?size:int -> bytestring -> unit
  val add_bits : t -> ?size:int -> int -> unit
  val add_utf8 : t -> ?size:int -> bytestring -> unit
  val add_literal_int : t -> ?size:int -> int -> unit
  val add_literal_utf8 : t -> ?size:int -> Stdlib.String.t -> unit
  val add_literal_string : t -> ?size:int -> Stdlib.String.t -> unit
  val commit : t -> bytestring
end

val to_transient : t -> Transient.t

val with_bytes :
  ?capacity:int -> (bytes -> (int, 'error) result) -> (t, 'error) result
