open Io
open Core
open Net
open Util
module Poll = Poll

type t = {
  poll : Poll.t;
  poll_timeout : Poll.Timeout.t;
  procs : (Fd.t, Process.t * [ `r | `w | `rw ]) Dashmap.t;
  fds : (Process.t, Fd.t) Dashmap.t;
}

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Socket.stream_socket * Addr.stream_addr | op ]
type read = [ `Read of int | op ]
type write = [ `Wrote of int | op ]
type sendfile = [ `Sent of int | op ]

(* basic api over t *)

let create () =
  {
    poll = Poll.create ();
    poll_timeout = Poll.Timeout.after 1_000_000L;
    procs = Dashmap.create ();
    fds = Dashmap.create ();
  }

(* pretty-printing *)

let mode_to_string mode = match mode with `r -> "r" | `rw -> "rw" | `w -> "w"

let pp ppf (t : t) =
  Format.fprintf ppf "[";
  Dashmap.iter t.procs (fun (fd, (proc, mode)) ->
      Format.fprintf ppf "%a:%a:%s," Fd.pp fd Pid.pp (Process.pid proc)
        (mode_to_string mode));
  Format.fprintf ppf "]"

let event_of_mode mode =
  match mode with
  | `r -> Poll.Event.read
  | `rw -> Poll.Event.read_write
  | `w -> Poll.Event.write

let mode_of_event event =
  match event with
  | Poll.Event.{ writable = false; readable = true } -> Some `r
  | Poll.Event.{ writable = true; readable = true } -> Some `rw
  | Poll.Event.{ writable = true; readable = false } -> Some `w
  | Poll.Event.{ writable = false; readable = false } -> None

(* NOTE(leostera): when we add a new Fd.t to our collection here, we need
   to update the current poller so that it knows of it.

   If we don't call [add_fd t fd] then we will never poll on [fd].
*)
let add_fd t fd mode =
  if Fd.is_closed fd then ()
  else
    let unix_fd = Fd.get fd |> Option.get in
    let flags = event_of_mode mode in
    Poll.set t.poll unix_fd flags

let register t proc mode fd =
  Log.debug (fun f -> f "Registering %a" Pid.pp (Process.pid proc));
  add_fd t fd mode;
  Dashmap.insert t.procs fd (proc, mode);
  Dashmap.insert t.fds proc fd

let unregister_process t proc =
  Log.debug (fun f -> f "Unregistering %a" Pid.pp (Process.pid proc));
  let fds = Dashmap.get_all t.fds proc in
  fds |> Dashmap.remove_all t.procs;
  List.iter
    (fun fd ->
      match Fd.get fd with
      | Some fd -> Poll.set t.poll fd Poll.Event.none
      | None -> ())
    fds;
  Dashmap.remove t.fds proc

let can_poll t =
  (not (Dashmap.is_empty t.procs)) && not (Dashmap.is_empty t.fds)

let poll t fn =
  match Poll.wait t.poll t.poll_timeout with
  | `Timeout -> ()
  | `Ok ->
      Poll.iter_ready t.poll ~f:(fun raw_fd event ->
          match mode_of_event event with
          | None -> ()
          | Some mode ->
              let fd = Fd.make raw_fd in
              let procs = Dashmap.get_all t.procs fd in
              let mode_and_flag (proc, mode') =
                Log.trace (fun f ->
                    f "io_poll(%a=%a,%a=%a): %a" Fd.pp fd Fd.pp fd Fd.Mode.pp
                      mode' Fd.Mode.pp mode Process.pp proc);
                let same_mode = Fd.Mode.equal mode' mode in
                match Fd.get fd with
                | Some fd' -> fd' = raw_fd && same_mode
                | _ -> false
              in
              procs |> List.filter mode_and_flag |> List.iter fn)

(* sockets api *)
let socket sock_domain sock_type =
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  Unix.set_nonblock fd;
  Fd.make fd

let close t fd =
  Log.trace (fun f -> f "closing %a" Fd.pp fd);
  Dashmap.remove_all t.procs [ fd ];
  Fd.close fd

let getaddrinfo host service =
  Log.debug (fun f -> f "getaddrinfo %s %s" host service);
  match Unix.getaddrinfo host service [] with
  | addr_info -> `Ok addr_info
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let listen (_t : t) ~reuse_addr ~reuse_port ~backlog addr =
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
  | () ->
      Log.debug (fun f ->
          f "listening to socket %a on %a" Fd.pp fd Addr.pp addr);
      Ok fd

let connect (_t : t) (addr : Addr.stream_addr) =
  Log.debug (fun f -> f "Connecting to: %a" Addr.pp addr);

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

let accept (socket : Fd.t) : accept =
  Fd.use ~op_name:"accept" socket @@ fun fd ->
  Log.debug (fun f -> f "Accepting client at fd=%a" Fd.pp socket);
  match Unix.accept ~cloexec:true fd with
  | raw_fd, client_addr ->
      Unix.set_nonblock raw_fd;
      let addr = Addr.of_unix client_addr in
      let fd = Fd.make raw_fd in
      Log.debug (fun f -> f "connected client with fd=%a" Fd.pp fd);
      `Connected (fd, addr)
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let read (fd : Fd.t) buf ~pos ~len : read =
  Fd.use ~op_name:"read" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Reading from fd=%a" Fd.pp fd);
  match UnixLabels.read unix_fd ~buf ~pos ~len with
  | len ->
      Log.debug (fun f -> f "read %d bytes from fd=%a" len Fd.pp fd);
      `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let write (fd : Fd.t) buf ~pos ~len : write =
  Fd.use ~op_name:"write" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Writing to fd=%a" Fd.pp fd);
  match UnixLabels.write unix_fd ~buf ~pos ~len with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_readv : Unix.file_descr -> Iovec.t -> int
  = "caml_riot_posix_readv"

let readv (fd : Fd.t) (iov : Iovec.t) : read =
  Fd.use ~op_name:"readv" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Readv-ing from fd=%a" Fd.pp fd);
  match riot_readv unix_fd iov with
  | len ->
      Log.debug (fun f -> f "read %d bytes from fd=%a" len Fd.pp fd);
      `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_writev : Unix.file_descr -> Iovec.t -> int
  = "caml_riot_posix_writev"

let writev (fd : Fd.t) (iov : Iovec.t) : write =
  Fd.use ~op_name:"readv" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Readv-ing from fd=%a" Fd.pp fd);
  match riot_writev unix_fd iov with
  | len ->
      Log.debug (fun f -> f "read %d bytes from fd=%a" len Fd.pp fd);
      `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_sendfile : Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "caml_riot_posix_sendfile"

let sendfile (file : Fd.t) (socket : Fd.t) ~off ~len : sendfile =
  Fd.use ~op_name:"senfile" file @@ fun file_fd ->
  Fd.use ~op_name:"senfile" socket @@ fun socket_fd ->
  match riot_sendfile file_fd socket_fd off len with
  | len -> `Sent len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

external riot_gettimeofday : unit -> int64 = "caml_riot_posix_gettimeofday"

let gettimeofday () = riot_gettimeofday ()
