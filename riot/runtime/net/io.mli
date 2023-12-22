open Core
open Util

type t

type accept =
  [ `Abort of Unix.error
  | `Connected of Socket.stream_socket * Addr.stream_addr
  | `Retry ]

type read = [ `Abort of Unix.error | `Read of int | `Retry ]
type write = [ `Abort of Unix.error | `Retry | `Wrote of int ]

val create : unit -> t
val pp : Format.formatter -> t -> unit
val register : t -> Process.t -> [ `r | `rw | `w ] -> Fd.t -> unit
val unregister_process : t -> Process.t -> unit
val poll : t -> (Process.t * Fd.Mode.t -> unit) -> unit
val can_poll : t -> bool
val close : t -> Fd.t -> unit

val getaddrinfo :
  string ->
  string ->
  [> `Abort of Unix.error | `Ok of Unix.addr_info list | `Retry ]

val listen :
  t ->
  reuse_addr:bool ->
  reuse_port:bool ->
  backlog:int ->
  Addr.stream_addr ->
  (Fd.t, 'a) result

val connect :
  t ->
  Addr.stream_addr ->
  [> `Abort of Unix.error | `Connected of Fd.t | `In_progress of Fd.t | `Retry ]

val accept : t -> Fd.t -> accept
val read : Fd.t -> bytes -> int -> int -> read
val write : Fd.t -> bytes -> int -> int -> write
