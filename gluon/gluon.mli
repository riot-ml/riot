open Io

module Fd : sig
  type t = Unix.file_descr

  val close : t -> unit
  val equal : t -> t -> bool
  val make : Unix.file_descr -> t
  val pp : Format.formatter -> t -> unit
  val seek : t -> int -> Unix.seek_command -> int
  val to_int : t -> int
end

module Non_zero_int : sig
  type t = int

  val make : int -> int option
end

module Token : sig
  type t

  val equal : t -> t -> bool
  val next : unit -> t
  val of_int : int -> t
  val pp : Format.formatter -> t -> unit
  val to_int : t -> int
end

module Interest : sig
  type t

  val add : t -> t -> t
  val is_readable : t -> bool
  val is_writable : t -> bool
  val readable : t
  val remove : t -> t -> t option
  val writable : t
end

module Event : sig
  module type Intf = sig
    type t

    val is_error : t -> bool
    val is_priority : t -> bool
    val is_read_closed : t -> bool
    val is_readable : t -> bool
    val is_writable : t -> bool
    val is_write_closed : t -> bool
    val token : t -> Token.t
  end

  type t

  val is_error : t -> bool
  val is_priority : t -> bool
  val is_read_closed : t -> bool
  val is_readable : t -> bool
  val is_writable : t -> bool
  val is_write_closed : t -> bool
  val make : (module Intf with type t = 'state) -> 'state -> t
  val token : t -> Token.t
end

module Sys : sig
  module Selector : sig
    type t

    val name : string
    val make : unit -> (t, [> `Noop ]) io_result

    val select :
      ?timeout:int64 ->
      ?max_events:int ->
      t ->
      (Event.t list, [> `Noop ]) io_result

    val register :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      (unit, [> `Noop ]) io_result

    val reregister :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      (unit, [> `Noop ]) io_result

    val deregister : t -> fd:Fd.t -> (unit, [> `Noop ]) io_result
  end

  module Event : sig
    type t
  end
end

module Source : sig
  module type Intf = sig
    type t

    val deregister : t -> Sys.Selector.t -> (unit, [> `Noop ]) io_result

    val register :
      t ->
      Sys.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result

    val reregister :
      t ->
      Sys.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result
  end

  type t = S : ((module Intf with type t = 'state) * 'state) -> t

  val deregister : t -> Sys.Selector.t -> (unit, [> `Noop ]) io_result
  val make : (module Intf with type t = 'a) -> 'a -> t

  val register :
    t -> Sys.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result

  val reregister :
    t -> Sys.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result
end

module File : sig
  type t = Unix.file_descr

  val pp : Format.formatter -> t -> unit
  val close : t -> unit
  val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  val write : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  val read_vectored : t -> Io.Iovec.t -> (int, [> `Noop ]) io_result
  val write_vectored : t -> Io.Iovec.t -> (int, [> `Noop ]) io_result
  val to_source : t -> Source.t
end

module Net : sig
  module Addr : sig
    type 't raw_addr = string
    type tcp_addr = [ `v4 | `v6 ] raw_addr
    type stream_addr

    val get_info : stream_addr -> (stream_addr list, [> `Noop ]) io_result
    val ip : stream_addr -> string
    val loopback : tcp_addr
    val of_addr_info : Unix.addr_info -> stream_addr option
    val of_unix : Unix.sockaddr -> stream_addr
    val of_uri : Uri.t -> (stream_addr, [> `Noop ]) io_result
    val parse : string -> (stream_addr, [> `Noop ]) io_result
    val port : stream_addr -> int
    val pp : Format.formatter -> stream_addr -> unit
    val tcp : tcp_addr -> int -> stream_addr
    val to_domain : stream_addr -> Unix.socket_domain
    val to_string : tcp_addr -> string
    val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
  end

  module Socket : sig
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket

    val pp : Format.formatter -> _ socket -> unit
    val close : _ socket -> unit
  end

  module Tcp_stream : sig
    type t = Socket.stream_socket

    val connect :
      Addr.stream_addr ->
      ([ `Connected of t | `In_progress of t ], [> `Noop ]) io_result

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
    val read_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result

    val sendfile :
      t -> file:Fd.t -> off:int -> len:int -> (int, [> `Noop ]) io_result

    val to_source : t -> Source.t

    val write :
      t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result

    val write_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  end

  module Tcp_listener : sig
    type t = Socket.listen_socket

    val accept : t -> (Tcp_stream.t * Addr.stream_addr, [> `Noop ]) io_result

    val bind :
      ?reuse_addr:bool ->
      ?reuse_port:bool ->
      ?backlog:int ->
      Addr.stream_addr ->
      (t, [> `Noop ]) io_result

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val to_source : t -> Source.t
  end
end

module Poll : sig
  type t

  val deregister : t -> Source.t -> (unit, [> `Noop ]) io_result
  val make : unit -> (t, [> `Noop ]) io_result

  val poll :
    ?max_events:int ->
    ?timeout:int64 ->
    t ->
    (Event.t list, [> `Noop ]) io_result

  val register :
    t -> Token.t -> Interest.t -> Source.t -> (unit, [> `Noop ]) io_result

  val reregister :
    t -> Token.t -> Interest.t -> Source.t -> (unit, [> `Noop ]) io_result
end
