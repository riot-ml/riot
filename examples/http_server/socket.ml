[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

module Logger = Logger.Make (struct
  let namespace = [ "socket" ]
end)

let trace, info, debug, warn, error = Logger.(trace, info, debug, warn, error)

type t = {
  fd : Unix.file_descr;
  addr : Unix.sockaddr;
  port : int;
  max_requests : int;
}

type connection = { fd : Unix.file_descr; client_addr : Unix.sockaddr }

let listen ~host ~port ~max_requests =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd addr;
  Unix.listen fd max_requests;
  info (fun f -> f "Listening on %s:%d" host port);
  { fd; addr; port; max_requests }

let rec accept (t : t) =
  yield ();
  match Unix.accept t.fd with
  | exception Unix.(Unix_error (EINTR, _, _)) -> accept t
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> accept t
  | fd, client_addr -> { fd; client_addr }

let close (conn : connection) = Unix.close conn.fd

module Flow = struct
  type Riot.Message.t += Wakeup_reader | Wakeup_writer
  type read_flow = [ `Read | `Close | `Yield ]

  type write_flow =
    [ `Close of int | `Write of Bigstringaf.t Faraday.iovec list | `Yield ]

  type t = {
    next_read_operation : unit -> read_flow;
    next_write_operation : unit -> write_flow;
    read : buf:Bigstringaf.t -> len:int -> unit;
    read_eof : buf:Bigstringaf.t -> len:int -> unit;
    report_write_result : [ `Ok of int | `Closed ] -> unit;
    yield_reader : (unit -> unit) -> unit;
    yield_writer : (unit -> unit) -> unit;
  }

  (** Write flow. Drives http/af to write to a Unix socket. *)
  let rec write (conn : connection) flow =
    match flow.next_write_operation () with
    | `Write io_vecs -> do_write conn flow io_vecs
    | `Close len -> do_close_write conn flow len
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    let writer_pid = self () in
    flow.yield_writer (fun () ->
        send writer_pid Wakeup_writer;
        yield ());
    let Wakeup_writer = receive () in
    write conn flow

  and do_write conn flow io_vecs =
    let rec write_all iovs total =
      match iovs with
      | [] -> `Ok total
      | Faraday.{ buffer; off; len } :: iovs ->
          let bytes = Bytes.create len in
          Bigstringaf.blit_to_bytes buffer ~src_off:off bytes ~dst_off:0 ~len;
          let len = Unix.write conn.fd bytes 0 len in
          debug (fun f -> f "wrote %d bytes: %s" len (Bytes.to_string bytes));
          write_all iovs (total + len)
    in
    let result = try write_all io_vecs 0 with End_of_file -> `Closed in
    flow.report_write_result result;
    write conn flow

  and do_close_write _conn _flow _len =
    debug (fun f -> f "closing %a" Pid.pp (self ()))

  (** Read flow. Drives http/af to read from a Unix socket. *)
  let rec read (conn : connection) flow =
    match flow.next_read_operation () with
    | `Read -> do_read conn flow
    | `Close -> do_close conn flow
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    let reader_pid = self () in
    flow.yield_reader (fun () ->
        send reader_pid Wakeup_reader;
        yield ());
    let Wakeup_reader = receive () in
    read conn flow

  and do_read conn flow =
    let bytes = Bytes.create 1024 in
    match Unix.read conn.fd bytes 0 (Bytes.length bytes) with
    | (exception Unix.(Unix_error (EINTR, _, _)))
    | (exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _))) ->
        yield ();
        read conn flow
    | exception _exn -> do_close conn flow
    | 0 ->
        flow.read_eof ~buf:Bigstringaf.empty ~len:0;
        read conn flow
    | len ->
        let buf = Bigstringaf.create len in
        Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
        debug (fun f -> f "read %d bytes" len);
        flow.read ~buf ~len;
        read conn flow

  and do_close conn _flow = close conn
end
