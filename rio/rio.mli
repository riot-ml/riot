type io_error =
  [ `Connection_closed
  | `Exn of exn
  | `No_info
  | `Unix_error of Unix.error
  | `Noop
  | `Eof
  | `Closed
  | `Process_down
  | `Timeout
  | `Would_block ]

type ('ok, 'err) io_result = ('ok, ([> io_error ] as 'err)) Stdlib.result

val pp_err : Format.formatter -> [< io_error ] -> unit

module Iovec : sig
  type iov = { ba : bytes; off : int; len : int }
  type t = iov array

  val with_capacity : int -> t
  val create : ?count:int -> size:int -> unit -> t
  val sub : ?pos:int -> len:int -> t -> t
  val length : t -> int
  val iter : t -> (iov -> unit) -> unit
  val of_bytes : bytes -> t
  val from_cstruct : Cstruct.t -> t
  val into_cstruct : t -> Cstruct.t
  val from_string : string -> t
  val from_buffer : Buffer.t -> t
  val into_string : t -> string
end

module type Write = sig
  type t

  val write : t -> buf:string -> (int, [> `Closed ]) io_result
  val write_owned_vectored : t -> bufs:Iovec.t -> (int, [> `Closed ]) io_result
  val flush : t -> (unit, [> `Closed ]) io_result
end

module Writer : sig
  type 'src write = (module Write with type t = 'src)
  type 'src t = Writer of ('src write * 'src)

  val of_write_src : 'a write -> 'a -> 'a t
end

module type Read = sig
  type t

  val read : t -> ?timeout:int64 -> bytes -> (int, [> `Closed ]) io_result
  val read_vectored : t -> Iovec.t -> (int, [> `Closed ]) io_result
end

module Reader : sig
  type 'src read = (module Read with type t = 'src)
  type 'src t = Reader of ('src read * 'src)

  val of_read_src : 'a read -> 'a -> 'a t
  val empty : unit t
end

val read :
  'a Reader.t -> ?timeout:int64 -> bytes -> (int, [> `Closed ]) io_result

val read_vectored : 'a Reader.t -> Iovec.t -> (int, [> `Closed ]) io_result
val read_to_end : 'a Reader.t -> buf:Buffer.t -> (int, [> `Closed ]) io_result
val write : 'src Writer.t -> buf:string -> (int, [> io_error ]) io_result
val write_all : 'a Writer.t -> buf:string -> (unit, [> `Closed ]) io_result

val write_owned_vectored :
  'a Writer.t -> bufs:Iovec.t -> (int, [> `Closed ]) io_result

val write_all_vectored :
  'a Writer.t -> bufs:Iovec.t -> (unit, [> `Closed ]) io_result

val flush : 'a Writer.t -> (unit, [> `Closed ]) io_result

module Cstruct : sig
  type t = Cstruct.t

  val to_writer : t -> t Writer.t
end

module Bytes : sig
  type t = bytes

  val empty : t
  val with_capacity : int -> t
  val length : t -> int
  val sub : t -> pos:int -> len:int -> t
  val of_string : string -> t
  val to_string : t -> string
  val split : ?max:int -> on:string -> t -> t list
  val join : t -> t -> t

  module Bytes_writer : sig
    type t
  end

  val to_writer : t -> Bytes_writer.t Writer.t
end

module Buffer : sig
  type t = Buffer.t

  val with_capacity : int -> t
  val length : t -> int
  val contents : t -> string
  val to_bytes : t -> bytes
  val to_writer : t -> t Writer.t
end
