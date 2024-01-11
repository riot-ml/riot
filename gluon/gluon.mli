module Sys : sig
  module Fd : sig
    module Mode : sig
      type t = [ `r | `rw | `w ]

      val equal : [> `r | `rw | `w ] -> [> `r | `rw | `w ] -> bool
      val pp : Format.formatter -> [< `r | `rw | `w ] -> unit
    end

    type fd = Unix.file_descr
    type state = [ `Closed | `Open of fd ]
    type t = { state : state Atomic.t }

    exception Already_closed of string

    val is_closed : t -> bool
    val is_open : t -> bool
    val get : t -> fd option
    val equal : t -> t -> bool
    val to_int : t -> int
    val pp : Format.formatter -> t -> unit
    val close : t -> unit
    val make : fd -> t
    val use : op_name:string -> t -> (fd -> 'a) -> 'a
    val seek : t -> int -> Unix.seek_command -> int

    module Set : sig
      type elt = t
      type t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
    end
  end

  module Addr : sig
    type 't raw_addr = string
    type tcp_addr = string
    type stream_addr = [ `Tcp of tcp_addr * int ]

    val loopback : tcp_addr
    val tcp : string -> 'a -> [> `Tcp of string * 'a ]

    val to_unix :
      [< `Tcp of tcp_addr * int ] -> Unix.socket_type * Unix.sockaddr

    val to_domain : [< `Tcp of 'a * 'b ] -> Unix.socket_domain
    val of_unix : Unix.sockaddr -> [> `Tcp of tcp_addr * int ]
    val pp : Format.formatter -> stream_addr -> unit
  end

  module Socket : sig
    type 'kind socket = Fd.t
    type listen_socket = Fd.t
    type stream_socket = Fd.t

    val pp : Format.formatter -> Fd.t -> unit
  end
end

open Sys

type 'rsc t

val create : unit -> 'rsc t
val pp : Format.formatter -> 'rsc t -> unit
val register : 'rsc t -> 'rsc -> [ `r | `rw | `w ] -> Fd.t -> unit
val unregister : 'rsc t -> 'rsc -> unit
val poll : 'rsc t -> ('rsc * Fd.Mode.t -> unit) -> unit
val can_poll : 'rsc t -> bool
val close : 'rsc t -> Fd.t -> unit

module Syscall : sig
  open Io

  type op = [ `Abort of Unix.error | `Retry ]
  type accept = [ `Connected of Socket.stream_socket * Addr.stream_addr | op ]
  type read = [ `Read of int | op ]
  type write = [ `Wrote of int | op ]
  type sendfile = [ `Sent of int | op ]

  val getaddrinfo :
    string ->
    string ->
    [> `Abort of Unix.error | `Ok of Unix.addr_info list | `Retry ]

  val listen :
    reuse_addr:bool ->
    reuse_port:bool ->
    backlog:int ->
    Addr.stream_addr ->
    (Fd.t, [> `Unix_error of Unix.error ]) result

  val connect :
    Addr.stream_addr ->
    [> `Abort of Unix.error
    | `Connected of Fd.t
    | `In_progress of Fd.t
    | `Retry ]

  val accept : Fd.t -> accept
  val read : Fd.t -> bytes -> pos:int -> len:int -> read
  val write : Fd.t -> bytes -> pos:int -> len:int -> write
  val readv : Fd.t -> Iovec.t -> read
  val writev : Fd.t -> Iovec.t -> write
  val sendfile : Fd.t -> Fd.t -> off:int -> len:int -> sendfile
  val gettimeofday : unit -> int64
end
