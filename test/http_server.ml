[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

let log, info, debug = Logs.(log, info, debug)

module Socket = struct
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

    let rec write (conn : connection) flow =
      info (fun f -> f "write loop");
      match flow.next_write_operation () with
      | `Write io_vecs -> do_write conn flow io_vecs
      | `Close len -> do_close_write conn flow len
      | `Yield -> do_yield conn flow

    and do_yield conn flow =
      info (fun f -> f "yield writer");
      flow.yield_writer (fun () -> send (self ()) Wakeup_writer);
      let Wakeup_writer = receive () in
      info (fun f -> f "awake writer");
      write conn flow

    and do_write conn flow io_vecs =
      info (fun f -> f "do write op");
      let rec write_all iovs total =
        match iovs with
        | [] -> `Ok total
        | Faraday.{ buffer; off; len } :: iovs ->
            let bytes = Bytes.create len in
            Bigstringaf.blit_to_bytes buffer ~src_off:off bytes ~dst_off:0 ~len;
            let len = Unix.write conn.fd bytes 0 len in
            info (fun f -> f "wrote %d bytes: %s" len (Bytes.to_string bytes));
            write_all iovs (total + len)
      in
      let result = try write_all io_vecs 0 with End_of_file -> `Closed in
      info (fun f -> f "finished writing, reporting...");
      flow.report_write_result result;
      write conn flow

    and do_close_write conn _flow len =
      info (fun f -> f "do write op");
      Logs.info (fun f -> f "wrote %d bytes" len);
      Unix.shutdown conn.fd Unix.SHUTDOWN_SEND

    let rec read (conn : connection) flow =
      match flow.next_read_operation () with
      | `Read -> do_read conn flow
      | `Close -> do_close conn flow
      | `Yield -> do_yield conn flow

    and do_yield conn flow =
      flow.yield_reader (fun () -> send (self ()) Wakeup_reader);
      let Wakeup_reader = receive () in
      read conn flow

    and do_read conn flow =
      let bytes = Bytes.create 1024 in
      match Unix.read conn.fd bytes 0 (Bytes.length bytes) with
      | exception Unix.(Unix_error (EINTR, _, _))
      | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
          yield ();
          read conn flow
      | exception _exn -> do_close conn flow
      | 0 ->
          flow.read_eof ~buf:Bigstringaf.empty ~len:0;
          read conn flow
      | len ->
          let buf = Bigstringaf.create len in
          Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
          Logs.info (fun f -> f "read %d bytes" len);
          flow.read ~buf ~len;
          read conn flow

    and do_close conn _flow = close conn
  end
end

module Server = struct
  module type Connector = sig
    type Message.t += Handshake of Socket.connection

    val start_link : unit -> (Pid.t, [> `Connector_error ]) result
    val handshake : conn:Socket.connection -> Pid.t -> unit
  end

  module type Protocol = sig
    val create_flow : unit -> Socket.Flow.t
  end

  module Tcp_connector (P : Protocol) : Connector = struct
    type Message.t += Handshake of Socket.connection

    let handle_handshake conn _state =
      info (fun f -> f "handling handshake");
      let flow = P.create_flow () in
      let _writer_pid = spawn_link (fun () -> Socket.Flow.write conn flow) in
      Socket.Flow.read conn flow

    let rec await_handshake state =
      let select = function Handshake _ -> Message.Take | _ -> Drop in
      match receive ~select () with
      | Handshake conn -> handle_handshake conn state
      | _ -> await_handshake state

    let start_link () =
      let pid = spawn_link (fun () -> await_handshake ()) in
      Ok pid

    let handshake ~conn pid = send pid (Handshake conn)
  end

  module Http_protocol : Protocol = struct
    open Socket.Flow

    let handler reqd =
      let req = Httpaf.Reqd.request reqd in
      Logs.info (fun f -> f "request: %a" Httpaf.Request.pp_hum req);
      let headers = Httpaf.Headers.of_list ["Content-Length", "3"] in
      let res = Httpaf.Response.create ~headers `OK in
      Logs.info (fun f -> f "response: %a" Httpaf.Response.pp_hum res);
      Httpaf.Reqd.respond_with_string reqd res "ok\n"

    let create_flow () =
      Logs.info (fun f -> f "creating http flow");
      let module S = Httpaf.Server_connection in
      let conn = S.create handler in

      {
        next_read_operation = (fun () -> S.next_read_operation conn);
        next_write_operation = (fun () -> S.next_write_operation conn);
        read = (fun ~buf ~len -> S.read conn buf ~off:0 ~len |> ignore);
        read_eof = (fun ~buf ~len -> S.read_eof conn buf ~off:0 ~len |> ignore);
        report_write_result = S.report_write_result conn;
        yield_reader = S.yield_reader conn;
        yield_writer = S.yield_writer conn;
      }
  end

  module Http_connector : Connector = Tcp_connector (Http_protocol)

  module Acceptor = struct
    type state = {
      socket : Socket.t;
      open_connections : (Socket.connection * Pid.t) list;
      connector : (module Connector);
    }

    let rec accept_loop state =
      let this = self () in
      info (fun f -> f "[acceptor=%a] Awaiting connection..." Pid.pp this);
      let conn = Socket.accept state.socket in
      let (module Connector) = state.connector in
      info (fun f -> f "[acceptor=%a] Accepted connection..." Pid.pp this);
      let (Ok pid) = Connector.start_link () in
      Connector.handshake ~conn pid;
      let state =
        { state with open_connections = (conn, pid) :: state.open_connections }
      in
      accept_loop state

    let start_link state =
      let pid = spawn_link (fun () -> accept_loop state) in
      Ok pid

    let child_spec ~socket (module C : Connector) =
      let state = { socket; open_connections = []; connector = (module C) } in
      Supervisor.child_spec ~start_link state

    module Sup = struct
      type state = {
        host : string;
        port : int;
        acceptors : int;
        connector : (module Connector);
      }

      let start_link { host; port; acceptors; connector } =
        let socket = Socket.listen ~host ~port ~max_requests:100 in
        let child_specs =
          List.init acceptors (fun _ -> child_spec ~socket connector)
        in
        Supervisor.start_link ~child_specs ()

      let child_spec ~host ~port ~acceptors ~connector =
        let state = { acceptors; host; port; connector } in
        Supervisor.child_spec ~start_link state
    end
  end

  let start_link ?(host = "0.0.0.0") ?(port = 2112) ?(acceptors = 20)
      ?(connector = (module Http_connector : Connector)) () =
    let child_specs =
      [ Acceptor.Sup.child_spec ~host ~port ~acceptors ~connector ]
    in
    Supervisor.start_link ~child_specs ()
end

let main () =
  let port = 2112 in
  info (fun f -> f "Starting server on port %d" port);
  let _server = Server.start_link ~port () in
  let rec loop () =
    yield ();
    loop ()
  in
  loop ()

let () =
  Logs.set_log_level (Some Debug);
  Riot.run @@ main
