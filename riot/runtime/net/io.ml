open Core
open Util
module Poll = Poll

type t = {
  poll : Poll.t;
  poll_timeout : Poll.Timeout.t;
  mutable poll_idx : int;
  fds : (Fd.t, int) Dashmap.t;
  procs : (Fd.t, Process.t * [ `r | `w | `rw ]) Dashmap.t;
}

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Socket.stream_socket * Addr.stream_addr | op ]
type read = [ `Read of int | op ]
type write = [ `Wrote of int | op ]

(* basic api over t *)

let create () =
  {
    poll = Poll.create ();
    poll_timeout = Poll.Timeout.After 500L;
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
      Format.fprintf ppf "%a:%a," Fd.pp fd Pid.pp (Process.pid proc))
    entries;
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
  if Fd.is_closed fd || Dashmap.has_key t.fds fd then ()
  else (
    Log.trace (fun f ->
        f "adding fd %d to poll slot %d" (Fd.to_int fd) t.poll_idx);
    let unix_fd = Fd.get fd |> Option.get in
    let flags = event_of_mode mode in
    Poll.set t.poll unix_fd flags;
    Dashmap.replace t.fds fd t.poll_idx;
    t.poll_idx <- t.poll_idx + 1)

let register t proc mode fd =
  add_fd t fd mode;
  Dashmap.insert t.procs fd (proc, mode)

let unregister_process t proc =
  let this_proc (_fd, (proc', _mode)) =
    Pid.equal (Process.pid proc) (Process.pid proc')
  in
  Dashmap.remove_by t.procs this_proc

let gc t =
  Dashmap.remove_by t.fds (fun (fd, _idx) -> Fd.is_open fd);
  Dashmap.remove_by t.procs (fun (_fd, (proc, _)) -> Process.is_waiting_io proc)

let can_poll t = not (Dashmap.is_empty t.procs)

let poll t fn =
  Poll.clear t.poll;
  gc t;
  let should_wait = ref false in
  Dashmap.iter t.procs (fun (fd, (_proc, mode)) ->
      match Fd.get fd with
      | None -> ()
      | Some unix_fd -> 
          should_wait := true;
          Poll.set t.poll unix_fd (event_of_mode mode));
  if not !should_wait then () 
  else
  match Poll.wait t.poll t.poll_timeout with
  | `Timeout -> ()
  | `Ok ->
      Poll.iter_ready t.poll ~f:(fun raw_fd event ->
          match mode_of_event event with
          | None -> ()
          | Some mode -> (
              match
                Dashmap.find_by t.fds (fun (fd, _idx) ->
                    match Fd.get fd with Some fd' -> fd' = raw_fd | _ -> false)
              with
              | None -> ()
              | Some (fd, _idx) ->
                  let mode_and_flag (fd', (proc, mode')) =
                    Log.trace (fun f ->
                        f "io_poll(%a=%a,%a=%a): %a" Fd.pp fd' Fd.pp fd
                          Fd.Mode.pp mode' Fd.Mode.pp mode Process.pp proc);
                    Fd.equal fd fd' && Fd.Mode.equal mode' mode
                  in
                  Dashmap.find_all_by t.procs mode_and_flag
                  |> List.iter (fun (_fd, proc) -> fn proc)))

(* sockets api *)
let socket sock_domain sock_type =
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  Unix.set_nonblock fd;
  Fd.make fd

let close (_t : t) fd =
  Log.trace (fun f -> f "closing %a" Fd.pp fd);
  Fd.close fd

let getaddrinfo host service =
  Log.error (fun f -> f "getaddrinfo %s %s" host service);
  match Unix.getaddrinfo host service [] with
  | addr_info -> `Ok addr_info
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let listen (_t : t) ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in
  Fd.use ~op_name:"listen" fd @@ fun sock ->
  Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
  Unix.bind sock sock_addr;
  Unix.listen sock backlog;
  Log.debug (fun f -> f "listening to socket %a on %a" Fd.pp fd Addr.pp addr);
  Ok fd

let connect (_t : t) (addr : Addr.stream_addr) =
  Log.error (fun f -> f "Connecting to: %a" Addr.pp addr);

  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in

  Fd.use ~op_name:"connect" fd @@ fun sock ->
  match Unix.connect sock sock_addr with
  | () -> `Connected fd
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> `In_progress fd
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let accept (_t : t) (socket : Fd.t) : accept =
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

let read (fd : Fd.t) buf off len : read =
  Fd.use ~op_name:"read" fd @@ fun fd ->
  match Unix.read fd buf off len with
  | len -> `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let write (fd : Fd.t) buf off len : write =
  Fd.use ~op_name:"write" fd @@ fun fd ->
  match Unix.write fd buf off len with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason
