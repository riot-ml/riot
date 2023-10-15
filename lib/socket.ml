open Riot_api

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "net"; "socket" ]
end)

type 'kind socket
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket
type listen_opts = { backlog : int; addr : Unix.inet_addr }
type connect_opts
type addr
type timeout = Infinity | Bounded of float
type unix_error = [ `Unix_error of Unix.error ]
type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

let default_listen_opts = { backlog = 128; addr = Io.stream_addr }

let listen ~port ?(opts = default_listen_opts) =
  let sch = Scheduler.get_current_scheduler () in
  Logger.trace (fun f -> f "Listening on 0.0.0.0:%d" port);
  Io.listen sch.io_tbl Io.{ addr = opts.addr; port; backlog = opts.backlog }

let accept ?(timeout = Infinity) socket =
  let sch = Scheduler.get_current_scheduler () in
  let pid = self () in
  Io.register_read sch.io_tbl socket.fd pid;
  let rec do_accept (socket : socket) =
    match receive () with
    | Io.Socket_read fd when socket.fd = fd -> (
        Logs.trace (fun f -> f "Accepting connections...");
        match Io.accept sch.io_tbl socket with
        | Abort -> Error `Connection_aborted
        | Retry -> do_accept socket
        | Connected conn -> Ok conn)
    | _ -> do_accept socket
  in
  let result = do_accept socket in
  Io.unregister_read sch.io_tbl socket.fd pid;
  result

let close socket =
  let sch = Scheduler.get_current_scheduler () in
  let this = self () in
  Logs.trace (fun f ->
      f "Process%a: Closing socket fd=%d" Pid.pp this (Obj.magic socket));
  Io.close sch.io_tbl socket

let controlling_process socket ~new_oner = Ok ()

let receive ?(timeout = Infinity) ~len socket =
  let sch = Scheduler.get_current_scheduler () in
  let pid = self () in
  Io.register_read sch.io_tbl conn.fd pid;
  let rec do_read (conn : connection) bytes off len =
    match receive () with
    | Io.Socket_read fd when conn.fd = fd -> (
        Logs.trace (fun f -> f "reading");
        match Io.read conn bytes off len with
        | Abort -> 0
        | Retry -> do_read conn bytes off len
        | Read len -> len)
    | _ -> failwith "unexpected message"
  in
  let result = do_read conn bytes off len in
  Io.unregister_read sch.io_tbl conn.fd pid;
  result

let send data socket =
  let sch = Scheduler.get_current_scheduler () in
  let pid = self () in
  Io.register_write sch.io_tbl conn.fd pid;
  let rec do_write (conn : connection) bytes off len =
    match receive () with
    | Io.Socket_write fd when conn.fd = fd -> (
        Logs.trace (fun f -> f "writing");
        match Io.write conn bytes off len with
        | Abort -> 0
        | Retry -> do_write conn bytes off len
        | Wrote len -> len)
    | _ -> failwith "unexpected message"
  in
  let result = do_write conn bytes off len in
  Io.unregister_write sch.io_tbl conn.fd pid;
  result

(*
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
  let rec write (conn : Unix.connection) flow =
    match flow.next_write_operation () with
    | `Write io_vecs -> do_write conn flow io_vecs
    | `Close len -> do_close_write conn flow len
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    debug (fun f -> f "yield writer");
    let writer_pid = self () in
    flow.yield_writer (fun () ->
        send writer_pid Wakeup_writer;
        yield ());
    let ref = Ref.make () in
    match receive ~ref () with
    | Wakeup_writer ->
        trace (fun f -> f "resumed writing");
        write conn flow
    | _ -> write conn flow

  and do_write conn flow io_vecs =
    debug (fun f -> f "do write");
    let rec write_all iovs total =
      match iovs with
      | [] -> `Ok total
      | Faraday.{ buffer; off; len } :: iovs ->
          let bytes = Bytes.create len in
          Bigstringaf.blit_to_bytes buffer ~src_off:off bytes ~dst_off:0 ~len;
          let len = Unix.write conn bytes 0 len in
          debug (fun f -> f "wrote %d bytes: %s" len (Bytes.to_string bytes));
          write_all iovs (total + len)
    in
    let result = try write_all io_vecs 0 with End_of_file -> `Closed in
    flow.report_write_result result;
    write conn flow

  and do_close_write conn _flow _len =
    debug (fun f -> f "closing %a" Pid.pp (self ()));
    Unix.close_connection conn

  (** Read flow. Drives http/af to read from a Unix socket. *)
  let rec read (conn : Unix.connection) flow =
    match flow.next_read_operation () with
    | `Read -> do_read conn flow
    | `Close -> do_close conn flow
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    debug (fun f -> f "yield reader");
    let reader_pid = self () in
    flow.yield_reader (fun () ->
        send reader_pid Wakeup_reader;
        yield ());
    let Wakeup_reader = receive () in
    trace (fun f -> f "resumed reading");
    read conn flow

  and do_read conn flow =
    debug (fun f -> f "do read");
    let bytes = Bytes.create 1024 in
    match Unix.read conn bytes 0 (Bytes.length bytes) with
    | 0 ->
        flow.read_eof ~buf:Bigstringaf.empty ~len:0;
        read conn flow
    | len ->
        let buf = Bigstringaf.create len in
        Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
        debug (fun f -> f "read %d bytes" len);
        flow.read ~buf ~len;
        read conn flow

  and do_close conn _flow =
    debug (fun f -> f "closing %a" Pid.pp (self ()));
    Unix.close_connection conn
end
*)
