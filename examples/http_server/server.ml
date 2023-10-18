[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

module Logger = Logger.Make (struct
  let namespace = [ "http_server" ]
end)

let trace, info, debug, warn, error = Logger.(trace, info, debug, warn, error)

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
  let rec write (conn : Net.stream_socket) flow =
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
    let Wakeup_writer = receive () in
    trace (fun f -> f "resumed writing");
    write conn flow

  and do_write conn flow io_vecs =
    debug (fun f -> f "do write");
    let rec write_all iovs total =
      match iovs with
      | [] -> `Ok total
      | Faraday.{ buffer; off; len } :: iovs -> (
          let data = Bigstringaf.sub buffer ~off ~len in
          match Socket.send data conn with
          | Error _ -> failwith "something went wrong"
          | Ok len ->
              debug (fun f ->
                  f "wrote %d bytes: %s" len (Bigstringaf.to_string data));
              write_all iovs (total + len))
    in
    let result = try write_all io_vecs 0 with End_of_file -> `Closed in
    flow.report_write_result result;
    write conn flow

  and do_close_write conn _flow _len =
    debug (fun f -> f "writer: closing %a" Pid.pp (self ()));
    Socket.close conn

  (** Read flow. Drives http/af to read from a Unix socket. *)
  let rec read (conn : Net.stream_socket) flow =
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
    match Socket.receive ~len:128 conn with
    | Ok data when Bigstringaf.length data = 0 ->
        flow.read_eof ~buf:Bigstringaf.empty ~len:0;
        read conn flow
    | Ok buf ->
        flow.read ~buf ~len:(Bigstringaf.length buf);
        read conn flow
    | Error `Closed ->
        error (fun f -> f "connection unexpectedly closed");
        do_close conn flow
    | Error `Timeout ->
        error (fun f -> f "timeout");
        do_close conn flow
    | Error (`Unix_error err) ->
        error (fun f -> f "unix error: %s" (Unix.error_message err));
        do_close conn flow

  and do_close conn _flow =
    debug (fun f -> f "reader: closing %a" Pid.pp (self ()));
    Socket.close conn
end

module type Connection = sig
  val start_link : Net.stream_socket -> (Pid.t, [> `connection_error ]) result
end

module type Protocol = sig
  val create_flow : unit -> Flow.t
end

module Tcp_connection (P : Protocol) : Connection = struct
  let handle_handshake conn =
    debug (fun f -> f "Protocol handshake initiated");
    let flow = P.create_flow () in
    let _reader =
      spawn_link (fun () ->
          debug (fun f -> f "spawned reader %a" Pid.pp (self ()));
          Flow.read conn flow)
    in
    let _writer =
      spawn_link (fun () ->
          debug (fun f -> f "spawned writer %a" Pid.pp (self ()));
          Flow.write conn flow)
    in
    receive () |> ignore

  let start_link conn =
    let pid = spawn_link (fun () -> handle_handshake conn) in
    debug (fun f -> f "spawned tcp_conenctor %a" Pid.pp pid);
    Ok pid
end

module Acceptor = struct
  type state = {
    socket : Net.listen_socket;
    open_connections : (Net.stream_socket * Pid.t) list;
    connection : (module Connection);
  }

  let rec accept_loop state =
    trace (fun f -> f "Awaiting connection...");
    let (Ok (conn, client_addr)) = Socket.accept state.socket in
    let (module Connection) = state.connection in
    let (Ok pid) = Connection.start_link conn in
    info (fun f -> f "%a: accepted connection" Net.Addr.pp client_addr);
    let state =
      { state with open_connections = (conn, pid) :: state.open_connections }
    in
    accept_loop state

  let start_link state =
    let pid =
      spawn_link (fun () ->
          process_flag (Trap_exit true);
          accept_loop state)
    in
    Ok pid

  let child_spec ~socket (module C : Connection) =
    let state = { socket; open_connections = []; connection = (module C) } in
    Supervisor.child_spec ~start_link state

  module Sup = struct
    type state = {
      port : int;
      acceptors : int;
      connection : (module Connection);
    }

    let start_link { port; acceptors; connection } =
      let (Ok socket) = Socket.listen ~port () in
      let child_specs =
        List.init acceptors (fun _ -> child_spec ~socket connection)
      in
      Supervisor.start_link ~child_specs ()

    let child_spec ~port ~acceptors ~connection =
      let state = { acceptors; port; connection } in
      Supervisor.child_spec ~start_link state
  end
end

let start_link ?(port = 2112) ?(acceptors = 1) connection () =
  let child_specs = [ Acceptor.Sup.child_spec ~port ~acceptors ~connection ] in
  Supervisor.start_link ~child_specs ()
