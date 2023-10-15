(* core types *)
module Fd : sig
  type t

  val to_int : t -> int
  val make : Unix.socket_domain -> Unix.socket_type -> t
  val get : t -> [ `Open of Unix.file_descr | `Closed ]
end = struct
  type t = [ `Open of Unix.file_descr | `Closed ] Rc.t

  let get t = Rc.get t
  let to_int t = match Rc.get t with `Open fd -> Obj.magic fd | `Closed -> -1

  let make sock_domain sock_type =
    let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
    Rc.make (`Open fd)
end

type 't raw_addr = string
type tcp_addr = [ `v4 | `v6 ] raw_addr
type stream_addr = [ `Tcp of tcp_addr * int ]
type 'kind socket = Fd.t
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket

type t = {
  fds : (Fd.t, unit) Dashmap.t;
  read : (Pid.t, Fd.t) Dashmap.t;
  write : (Pid.t, Fd.t) Dashmap.t;
}

(* auxiliary types *)

type awake = { read : (Pid.t * Fd.t) list; write : (Pid.t * Fd.t) list }

(* operation types *)

type accept = Abort | Retry | Connected of stream_socket
type read = Abort | Retry | Read of int
type write = Abort | Retry | Wrote of int

(* basic api over t *)

let create () =
  {
    fds = Dashmap.create 1024;
    read = Dashmap.create 1024;
    write = Dashmap.create 1024;
  }

let add_fd t fd = Dashmap.insert t.fds fd ()
let register_read (t : t) fd pid = Dashmap.insert t.read pid fd
let unregister_read (t : t) fd pid = Dashmap.remove t.read pid fd
let register_write (t : t) fd pid = Dashmap.insert t.write pid fd
let unregister_write (t : t) fd pid = Dashmap.remove t.write pid fd

(* pretty-printing *)

let rec pp ppf (t : t) =
  Format.fprintf ppf "{ fds=%a; read=%a; write=%a }" pp_fds_table t.fds
    pp_pid_table t.read pp_pid_table t.write

and pp_fds_table ppf tbl =
  let entries = Dashmap.entries tbl in
  Format.fprintf ppf "[";
  List.iter (fun (fd, _) -> Format.fprintf ppf "%d," (Obj.magic fd)) entries;
  Format.fprintf ppf "]"

and pp_pid_table ppf tbl =
  let entries = Dashmap.entries tbl in
  Format.fprintf ppf "[";
  List.iter
    (fun (pid, fd) -> Format.fprintf ppf "%a:%d," Pid.pp pid (Obj.magic fd))
    entries;
  Format.fprintf ppf "]"

and pp_socket ppf (socket : _ socket) =
  Format.fprintf ppf "Socket { fd=%d; }" (Fd.to_int socket)

and pp_addr ppf (addr : stream_addr) =
  match addr with `Tcp (host, port) -> Format.fprintf ppf "%s:%d" host port

(* sockets api *)

let addr_to_unix_sockaddr addr =
  match addr with
  | `Tcp (host, port) ->
      let addr = Unix.inet_addr_of_string host in
      (Unix.SOCK_STREAM, Unix.ADDR_INET (addr, port))

let addr_to_unix_socket_domain addr =
  match addr with `Tcp (_host, _) -> Unix.PF_INET

let listen (t : t) ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = addr_to_unix_socket_domain addr in
  let sock_type, sock_addr = addr_to_unix_sockaddr addr in
  let fd = Fd.make sock_domain sock_type in
  match Fd.get fd with
  | `Open sock ->
      Unix.set_nonblock sock;
      Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
      Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
      Unix.bind sock sock_addr;
      Unix.listen sock backlog;
      Logs.trace (fun f -> f "listening to socket: %a" pp_addr addr);
      add_fd t fd;
      Ok fd
  | `Closed ->
      Logs.error (fun f -> f "socket closed while opening");
      Error `Closed

let select (t : t) =
  let select t ~read ~write =
    try UnixLabels.select ~read ~write ~except:[] ~timeout:0.001
    with Unix.Unix_error (Unix.EBADF, _, _) ->
      Logs.warn (fun f -> f "%a" pp t);
      ([], [], [])
  in
  let reads = Dashmap.entries t.read in
  let writes = Dashmap.entries t.write in
  let read, write =
    let fds = List.map (fun (_, fd) -> fd) in
    let read, write, _ = select t ~read:(fds reads) ~write:(fds writes) in
    let read = List.filter (fun (_, fd) -> List.mem fd read) reads in
    let write = List.filter (fun (_, fd) -> List.mem fd write) writes in
    (read, write)
  in
  { read; write }

let close (t : t) fd =
  let rm_fd (fd', ()) = if fd = fd' then None else Some (fd', ()) in
  let rm_pid (pid, fd') = if fd = fd' then None else Some (pid, fd') in
  Dashmap.update t.fds rm_fd;
  Dashmap.update t.read rm_pid;
  Dashmap.update t.write rm_pid;
  Unix.close fd

let accept (t : t) (socket : socket) : accept =
  match Unix.accept socket.fd with
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> Retry
  | exception _ -> Abort
  | fd, client_addr ->
      Logs.trace (fun f -> f "connected client with fd=%d" (Obj.magic fd));
      add_fd t fd;
      Connected { fd; client_addr }

let read (conn : connection) buf off len : read =
  match Unix.read conn.fd buf off len with
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> Retry
  | exception _ -> Abort
  | len -> Read len

let write (conn : connection) buf off len : write =
  match Unix.write conn.fd buf off len with
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> Retry
  | exception _ -> Abort
  | len -> Wrote len
