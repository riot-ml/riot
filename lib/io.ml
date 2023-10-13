type fd = Unix.file_descr

type socket = {
  fd : fd;
  addr : Unix.sockaddr;
  addr_str : string;
  port : int;
  max_requests : int;
}

type listen = { pid : Pid.t; host : string; port : int; max_requests : int }
type connection = { fd : fd; client_addr : Unix.sockaddr }
type accept = Abort | Retry | Connected of connection
type read = Abort | Retry | Read of int
type write = Abort | Retry | Wrote of int
type Message.t += Socket_read of fd | Socket_write of fd | Socket_except of fd

module PidSet = Set.Make (struct
  type t = Pid.t

  let compare = Pid.compare
end)

type t = {
  read : (fd, PidSet.t) Hashtbl.t;
  write : (fd, PidSet.t) Hashtbl.t;
  except : (fd, PidSet.t) Hashtbl.t;
}

let rec pp ppf (t : t) =
  Format.fprintf ppf "{ read=%a; write=%a; except=%a }" pp_pid_table t.read
    pp_pid_table t.write pp_pid_table t.except

and pp_pid_table ppf tbl =
  let entries = Hashtbl.to_seq_keys tbl in
  Format.fprintf ppf "[";
  Seq.iter (fun fd -> Format.fprintf ppf "%d," (Obj.magic fd)) entries;
  Format.fprintf ppf "]"

and pp_socket ppf (socket : socket) =
  Format.fprintf ppf "Socket { fd=%d; addr=%s:%d; max_req=%d }"
    (Obj.magic socket.fd) socket.addr_str socket.port socket.max_requests

let create () =
  {
    read = Hashtbl.create 1024;
    write = Hashtbl.create 1024;
    except = Hashtbl.create 1024;
  }

type awake = {
  read : (fd * Pid.t list) list;
  write : (fd * Pid.t list) list;
  except : (fd * Pid.t list) list;
}

let register_read (t : t) fd pid =
  Logs.trace (fun f -> f "registered for reading: %a" Pid.pp pid);
  let old_set =
    match Hashtbl.find_opt t.read fd with
    | None -> PidSet.empty
    | Some old_set -> old_set
  in
  let pid_set = PidSet.add pid old_set in
  Hashtbl.replace t.read fd pid_set

let register_write (t : t) fd pid =
  Logs.trace (fun f -> f "registered for writing: %a" Pid.pp pid);
  let old_set =
    match Hashtbl.find_opt t.write fd with
    | None -> PidSet.empty
    | Some old_set -> old_set
  in
  let pid_set = PidSet.add pid old_set in
  Hashtbl.replace t.write fd pid_set

let register_except (t : t) fd pid =
  Logs.trace (fun f -> f "registered for exceptional: %a" Pid.pp pid);
  let old_set =
    match Hashtbl.find_opt t.except fd with
    | None -> PidSet.empty
    | Some old_set -> old_set
  in
  let pid_set = PidSet.add pid old_set in
  Hashtbl.replace t.except fd pid_set

let get_fds pid_tbl = pid_tbl |> Hashtbl.to_seq_keys |> List.of_seq

let find_pids fds pid_tbl =
  List.map
    (fun fd ->
      let pid_set =
        match Hashtbl.find_opt pid_tbl fd with
        | None -> PidSet.empty
        | Some pid_set -> pid_set
      in
      (fd, PidSet.to_list pid_set))
    fds

let rec select (t : t) =
  try
    let read = get_fds t.read in
    let write = get_fds t.write in
    let except = get_fds t.except in
    let read, write, except =
      UnixLabels.select ~read ~write ~except ~timeout:0.0
    in
    {
      read = find_pids read t.read;
      write = find_pids write t.write;
      except = find_pids except t.except;
    }
  with
  | Unix.Unix_error (Unix.EBADF, _, _) ->
      Logs.error (fun f -> f "ebadf: %a" pp t);
      { read = []; write = []; except = [] }
  | _exn -> { read = []; write = []; except = [] }

let close (t : t) fd =
  Hashtbl.remove t.read fd;
  Hashtbl.remove t.write fd;
  Hashtbl.remove t.except fd;
  Unix.close fd

let listen (t : t) { pid; host; port; max_requests } =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd addr;
  Unix.listen fd max_requests;
  let socket = { fd; addr; addr_str = host; port; max_requests } in
  Logs.trace (fun f -> f "listening to socket: %a" pp_socket socket);
  register_read t socket.fd pid;
  socket

let accept (socket : socket) : accept =
  match Unix.accept socket.fd with
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> Retry
  | exception _ -> Abort
  | fd, client_addr -> Connected { fd; client_addr }

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
