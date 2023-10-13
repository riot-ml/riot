type fd = Unix.file_descr
type socket = { fd : fd; addr : Unix.sockaddr; port : int; max_requests : int }
type listen = { pid : Pid.t; host : string; port : int; max_requests : int }
type connection = { fd : fd; client_addr : Unix.sockaddr }
type accept = Retry | Connected of connection
type read = Retry | Read of int
type write = Retry | Wrote of int
type Message.t += Socket_read of fd | Socket_write of fd | Socket_except of fd

type t = {
  fds : (Unix.file_descr, socket) Hashtbl.t;
  read : (socket, Pid.t) Hashtbl.t;
  write : (socket, Pid.t) Hashtbl.t;
  except : (socket, Pid.t) Hashtbl.t;
}

let create () =
  {
    fds = Hashtbl.create 1024;
    read = Hashtbl.create 1024;
    write = Hashtbl.create 1024;
    except = Hashtbl.create 1024;
  }

type awake = {
  read : (fd * Pid.t list) list;
  write : (fd * Pid.t list) list;
  except : (fd * Pid.t list) list;
}

let register_socket t (socket : socket) = Hashtbl.add t.fds socket.fd socket

let register_read t socket pid =
  register_socket t socket;
  Hashtbl.add t.read socket pid

let register_write t socket pid =
  register_socket t socket;
  Hashtbl.add t.write socket pid

let register_except t socket pid =
  register_socket t socket;
  Hashtbl.add t.except socket pid

let get_fds pid_tbl =
  pid_tbl |> Hashtbl.to_seq_keys
  |> Seq.map (fun ({ fd; _ } : socket) -> fd)
  |> List.of_seq

let find_pids fds fd_tbl pid_tbl =
  List.map
    (fun fd ->
      let socket : socket = Hashtbl.find fd_tbl fd in
      (socket.fd, Hashtbl.find_all pid_tbl socket))
    fds

let select (t : t) =
  let read = get_fds t.read in
  let write = get_fds t.write in
  let except = get_fds t.except in
  let read, write, except =
    UnixLabels.select ~read ~write ~except ~timeout:0.0
  in
  {
    read = find_pids read t.fds t.read;
    write = find_pids write t.fds t.write;
    except = find_pids except t.fds t.except;
  }

let close (t : t) fd =
  Unix.close fd;
  (match Hashtbl.find_opt t.fds fd with
  | None -> ()
  | Some socket ->
      Hashtbl.remove t.read socket;
      Hashtbl.remove t.write socket;
      Hashtbl.remove t.except socket);
  Hashtbl.remove t.fds fd

let listen (t : t) { pid; host; port; max_requests } =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd addr;
  Unix.listen fd max_requests;
  let socket = { fd; addr; port; max_requests } in
  register_read t socket pid;
  socket

let accept (socket : socket) : accept =
  match Unix.accept socket.fd with
  | (exception Unix.(Unix_error (EINTR, _, _)))
  | (exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _))) ->
      Retry
  | fd, client_addr -> Connected { fd; client_addr }

let read (conn : connection) buf off len : read =
  match Unix.read conn.fd buf off len with
  | (exception Unix.(Unix_error (EINTR, _, _)))
  | (exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _))) ->
      Retry
  | len -> Read len

let write (conn : connection) buf off len : write =
  match Unix.write conn.fd buf off len with
  | (exception Unix.(Unix_error (EINTR, _, _)))
  | (exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _))) ->
      Retry
  | len -> Wrote len
