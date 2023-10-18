module Poll = Iomux.Poll

type t = {
  poll : Poll.t;
  poll_timeout : Poll.ppoll_timeout;
  mutable poll_idx : int;
  fds : (Fd.t, int) Dashmap.t;
  procs : (Fd.t, Process.t) Dashmap.t;
}

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Net.stream_socket * Net.Addr.stream_addr | op ]
type read = [ `Read of int | op ]
type write = [ `Wrote of int | op ]

(* basic api over t *)

let create () =
  {
    poll = Poll.create ~maxfds:1024 ();
    poll_timeout = Poll.Nanoseconds 10L;
    poll_idx = 0;
    fds = Dashmap.create 1024;
    procs = Dashmap.create 1024;
  }

(* pretty-printing *)

let rec pp ppf (t : t) =
  Format.fprintf ppf "{ fds=%a; procs=%a }" pp_fds_table t.fds pp_proc_table
    t.procs

and pp_fds_table ppf tbl =
  let entries = Dashmap.entries tbl in
  Format.fprintf ppf "[";
  List.iter (fun (fd, _) -> Format.fprintf ppf "%a," Fd.pp fd) entries;
  Format.fprintf ppf "]"

and pp_proc_table ppf tbl =
  let entries = Dashmap.entries tbl in
  Format.fprintf ppf "[";
  List.iter
    (fun (fd, proc) ->
      Format.fprintf ppf "%a:%a," Fd.pp fd Pid.pp Process.(proc.pid))
    entries;
  Format.fprintf ppf "]"

(* NOTE(leostera): when we add a new Fd.t to our collection here, we need
   to update the current poller so that it knows of it.

   If we don't call [add_fd t fd] then we will never poll on [fd].
*)
let add_fd t fd =
  match Fd.get fd with
  | Some unix_fd ->
      let flags = Poll.Flags.(pollin + pollout) in
      Poll.set_index t.poll t.poll_idx unix_fd flags;
      Dashmap.replace t.fds fd t.poll_idx;
      t.poll_idx <- t.poll_idx + 1
  | None -> ()

let rm_fd t fd =
  let poll_idx = Dashmap.find t.fds fd |> Option.get in
  Poll.invalidate_index t.poll poll_idx

let register t proc fd =
  add_fd t fd;
  Dashmap.replace t.procs fd proc

let unregister t fd =
  rm_fd t fd;
  Dashmap.remove_by t.procs (fun (fd', _proc) -> Fd.equal fd fd')

let unregister_process t proc =
  let open Process in
  let this_proc (_, proc') = Pid.equal proc.pid proc'.pid in
  match Dashmap.find_by t.procs this_proc with
  | Some (fd, _proc) -> Dashmap.remove_by t.procs this_proc
  | None -> ()

let poll t fn =
  let ready_count = Poll.ppoll_or_poll t.poll t.poll_idx t.poll_timeout in
  Poll.iter_ready t.poll ready_count @@ fun _idx raw_fd fd_flags ->
  let flag =
    match
      (Poll.Flags.(mem fd_flags pollin), Poll.Flags.(mem fd_flags pollout))
    with
    | true, false -> `r
    | false, true -> `w
    | _ -> `rw
  in
  match
    Dashmap.find_by t.fds (fun (fd, _idx) ->
        match Fd.get fd with Some fd' -> fd' = raw_fd | None -> false)
  with
  | None ->
      Logs.error (fun f ->
          f "io_poll: couldn't find raw_fd %d" (Obj.magic raw_fd))
  | Some (fd, _idx) -> (
      match Dashmap.find_by t.procs (fun (fd', _proc) -> Fd.equal fd fd') with
      | None -> ()
      | Some (_fd, proc) -> fn flag proc)

(* sockets api *)
let close (t : t) fd =
  Logs.trace (fun f -> f "closing %a" Fd.pp fd);
  Fd.close fd

let listen (t : t) ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = Net.Addr.to_domain addr in
  let sock_type, sock_addr = Net.Addr.to_unix addr in
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  let fd = Fd.make ~release:(unregister t) fd in
  Fd.use ~op_name:"listen" fd @@ fun sock ->
  Unix.set_nonblock sock;
  Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
  Unix.bind sock sock_addr;
  Unix.listen sock backlog;
  Logs.trace (fun f ->
      f "listening to socket %a on %a" Fd.pp fd Net.Addr.pp addr);
  Ok fd

let accept (t : t) (socket : Fd.t) : accept =
  Fd.use ~op_name:"accept" socket @@ fun fd ->
  match Unix.accept ~cloexec:true fd with
  | raw_fd, client_addr ->
      let addr = Net.Addr.of_unix client_addr in
      let fd = Fd.make ~release:(unregister t) raw_fd in
      Logs.trace (fun f -> f "connected client with fd=%a" Fd.pp fd);
      `Connected (fd, addr)
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let read (conn : Net.stream_socket) buf off len : read =
  Fd.use ~op_name:"read" conn @@ fun fd ->
  match Unix.read fd buf off len with
  | len -> `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let write (conn : Net.stream_socket) buf off len : write =
  Fd.use ~op_name:"write" conn @@ fun fd ->
  match Unix.write fd buf off len with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason
