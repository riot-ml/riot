open Io
open Sys

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Socket.stream_socket * Addr.stream_addr | op ]
type read = [ `Read of int | op ]
type write = [ `Wrote of int | op ]
type sendfile = [ `Sent of int | op ]

(* sockets api *)
let socket sock_domain sock_type =
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  Unix.set_nonblock fd;
  Fd.make fd

let getaddrinfo host service =
  match Unix.getaddrinfo host service [] with
  | addr_info -> `Ok addr_info
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let listen ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in
  Fd.use ~op_name:"listen" fd @@ fun sock ->
  match
    Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
    Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
    Unix.bind sock sock_addr;
    Unix.listen sock backlog
  with
  | exception Unix.(Unix_error (reason, _, _)) -> Error (`Unix_error reason)
  | () -> Ok fd

let connect addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in

  Fd.use ~op_name:"connect" fd @@ fun sock ->
  match Unix.connect sock sock_addr with
  | () -> `Connected fd
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> `In_progress fd
  | exception
      Unix.(Unix_error ((ENOTCONN | EINTR | EAGAIN | EWOULDBLOCK), _, _)) ->
      `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let accept socket =
  Fd.use ~op_name:"accept" socket @@ fun fd ->
  match Unix.accept ~cloexec:true fd with
  | raw_fd, client_addr ->
      Unix.set_nonblock raw_fd;
      let addr = Addr.of_unix client_addr in
      let fd = Fd.make raw_fd in
      `Connected (fd, addr)
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let read fd buf ~pos ~len =
  Fd.use ~op_name:"read" fd @@ fun unix_fd ->
  match UnixLabels.read unix_fd ~buf ~pos ~len with
  | len -> `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let write fd buf ~pos ~len =
  Fd.use ~op_name:"write" fd @@ fun unix_fd ->
  match UnixLabels.write unix_fd ~buf ~pos ~len with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_readv : Unix.file_descr -> Iovec.t -> int
  = "caml_riot_posix_readv"

let readv fd iov =
  Fd.use ~op_name:"readv" fd @@ fun unix_fd ->
  match riot_readv unix_fd iov with
  | len -> `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_writev : Unix.file_descr -> Iovec.t -> int
  = "caml_riot_posix_writev"

let writev fd iov =
  Fd.use ~op_name:"readv" fd @@ fun unix_fd ->
  match riot_writev unix_fd iov with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_sendfile : Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "caml_riot_posix_sendfile"

let sendfile file socket ~off ~len =
  Fd.use ~op_name:"senfile" file @@ fun file_fd ->
  Fd.use ~op_name:"senfile" socket @@ fun socket_fd ->
  match riot_sendfile file_fd socket_fd off len with
  | len -> `Sent len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_gettimeofday : unit -> int64 = "caml_riot_posix_gettimeofday"

let gettimeofday () = riot_gettimeofday ()
