module Poll = Iomux.Poll

type t = {
  poll : Poll.t;
  poll_timeout : Poll.ppoll_timeout;
  mutable poll_idx : int;
  fds : (Fd.t, int) Dashmap.t;
  procs : (Fd.t, Process.t * [ `r | `w | `rw ]) Dashmap.t;
}

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Net.stream_socket * Addr.stream_addr | op ]
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
  List.iter (fun (fd, idx) -> Format.fprintf ppf "%a/%d," Fd.pp fd idx) entries;
  Format.fprintf ppf "]"

and pp_proc_table ppf tbl =
  let entries = Dashmap.entries tbl in
  Format.fprintf ppf "[";
  List.iter
    (fun (fd, (proc, _mode)) ->
      Format.fprintf ppf "%a:%a," Fd.pp fd Pid.pp Process.(proc.pid))
    entries;
  Format.fprintf ppf "]"

let mode_of_flags flags =
  match (Poll.Flags.(mem flags pollin), Poll.Flags.(mem flags pollout)) with
  | true, false -> Some `r
  | false, true -> Some `w
  | true, true -> Some `rw
  | _, _ -> None

let flags_of_mode mode =
  match mode with
  | `rw -> Poll.Flags.(pollin + pollout)
  | `r -> Poll.Flags.(pollin)
  | `w -> Poll.Flags.(pollout)

(* NOTE(leostera): when we add a new Fd.t to our collection here, we need
   to update the current poller so that it knows of it.

   If we don't call [add_fd t fd] then we will never poll on [fd].
*)
let add_fd t fd mode =
  if Fd.is_closed fd || Dashmap.has_key t.fds fd then ()
  else (
    Logs.trace (fun f ->
        f "adding fd %d to poll slot %d" (Fd.to_int fd) t.poll_idx);
    let unix_fd = Fd.get fd |> Option.get in
    let flags = flags_of_mode mode in
    Poll.set_index t.poll t.poll_idx unix_fd flags;
    Dashmap.replace t.fds fd t.poll_idx;
    t.poll_idx <- t.poll_idx + 1)

let register t proc mode fd =
  add_fd t fd mode;
  Dashmap.insert t.procs fd (proc, mode)

let unregister_process t proc =
  let open Process in
  let this_proc (_fd, (proc', _mode)) = Pid.equal proc.pid proc'.pid in
  Dashmap.remove_by t.procs this_proc

let gc t =
  Dashmap.remove_by t.fds (fun (fd, idx) ->
      let is_open = Fd.is_open fd in
      if not is_open then Poll.invalidate_index t.poll idx;
      is_open)

let poll t fn =
  gc t;
  let ready_count = Poll.ppoll_or_poll t.poll t.poll_idx t.poll_timeout in
  Poll.iter_ready t.poll ready_count @@ fun _idx raw_fd fd_flags ->
  match mode_of_flags fd_flags with
  | None ->
      Logs.trace (fun f ->
          let buf = Buffer.create 128 in
          let fmt = Format.formatter_of_buffer buf in
          if Poll.Flags.(mem fd_flags pollin) then Format.fprintf fmt "pollin,";
          if Poll.Flags.(mem fd_flags pollout) then
            Format.fprintf fmt "pollout,";
          if Poll.Flags.(mem fd_flags pollerr) then
            Format.fprintf fmt "pollerr,";
          if Poll.Flags.(mem fd_flags pollhup) then
            Format.fprintf fmt "pollhup,";
          if Poll.Flags.(mem fd_flags pollnval) then
            Format.fprintf fmt "pollnval,";
          if Poll.Flags.(mem fd_flags pollpri) then Format.fprintf fmt "pollpri";
          Format.fprintf fmt "%!";
          f "io_poll(%d): unexpected flags: %s" (Obj.magic raw_fd)
            (Buffer.contents buf))
  | Some mode -> (
      match
        Dashmap.find_by t.fds (fun (fd, _idx) ->
            match Fd.get fd with Some fd' -> fd' = raw_fd | _ -> false)
      with
      | None -> ()
      | Some (fd, _idx) ->
          let mode_and_flag (fd', (proc, mode')) =
            Logs.trace (fun f ->
                f "io_poll(%a=%a,%a=%a): %a" Fd.pp fd' Fd.pp fd Fd.Mode.pp mode'
                  Fd.Mode.pp mode Process.pp proc);
            Fd.equal fd fd' && Fd.Mode.equal mode' mode
          in
          Dashmap.find_all_by t.procs mode_and_flag
          |> List.iter (fun (_fd, proc) -> fn proc))

(* sockets api *)
let close (_t : t) fd =
  Logs.trace (fun f -> f "closing %a" Fd.pp fd);
  Fd.close fd

let getaddrinfo host service =
  match Unix.getaddrinfo host service [] with
  | addr_info -> `Ok addr_info
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let listen (_t : t) ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  let fd = Fd.make fd in
  Fd.use ~op_name:"listen" fd @@ fun sock ->
  Unix.set_nonblock sock;
  Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
  Unix.bind sock sock_addr;
  Unix.listen sock backlog;
  Logs.trace (fun f -> f "listening to socket %a on %a" Fd.pp fd Addr.pp addr);
  Ok fd

let accept (_t : t) (socket : Fd.t) : accept =
  Fd.use ~op_name:"accept" socket @@ fun fd ->
  match Unix.accept ~cloexec:true fd with
  | raw_fd, client_addr ->
      let addr = Addr.of_unix client_addr in
      let fd = Fd.make raw_fd in
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
