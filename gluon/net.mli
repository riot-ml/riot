open Gluon_common

module Addr : sig
  type tcp_addr
  type stream_addr

  val get_info : stream_addr -> stream_addr list io_result
  val ip : stream_addr -> string
  val loopback : tcp_addr
  val of_addr_info : Unix.addr_info -> stream_addr option
  val of_unix : Unix.sockaddr -> stream_addr
  val of_uri : Uri.t -> stream_addr io_result
  val parse : string -> stream_addr io_result
  val port : stream_addr -> int
  val pp : Format.formatter -> stream_addr -> unit
  val tcp : tcp_addr -> int -> stream_addr
  val to_domain : stream_addr -> Unix.socket_domain
  val to_string : tcp_addr -> string
  val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
end

module Socket : sig
  type 'kind socket
  type listen_socket = [ `listen ] socket
  type stream_socket = [ `stream ] socket

  val pp : Format.formatter -> _ socket -> unit
end

module Tcp_stream : sig
  type t

  val pp : Format.formatter -> t -> unit
  val read : t -> ?pos:int -> ?len:int -> bytes -> int io_result
  val write : t -> ?pos:int -> ?len:int -> bytes -> int io_result
  val readv : t -> Io.Iovec.t -> int io_result
  val writev : t -> Io.Iovec.t -> int io_result
  val sendfile : t -> file:Fd.t -> off:int -> len:int -> int io_result
  val to_source : t -> Source.t
end

module Tcp_listener : sig
  type t

  val bind :
    ?reuse_addr:bool ->
    ?reuse_port:bool ->
    ?backlog:int ->
    Addr.stream_addr ->
    t io_result

  val accept : t -> (Tcp_stream.t * Addr.stream_addr) io_result
  val to_source : t -> Source.t
end
