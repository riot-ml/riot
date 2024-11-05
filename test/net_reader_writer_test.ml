open Riot

type Message.t += Received of string

(* rudimentary tcp echo server *)
let server port socket =
  Logger.debug (fun f -> f "Started server on %d" port);
  process_flag (Trap_exit true);
  let conn, addr = Net.Tcp_listener.accept socket |> Result.get_ok in
  Logger.debug (fun f ->
      f "Accepted client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn);
  let close () =
    Net.Tcp_stream.close conn;
    Logger.debug (fun f ->
        f "Closed client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn)
  in

  let reader = Net.Tcp_stream.to_reader conn in
  let writer = Net.Tcp_stream.to_writer conn in

  let bufs = IO.Iovec.with_capacity 1024 in
  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match IO.read_vectored reader bufs with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let bufs = IO.Iovec.sub ~len bufs in
        match IO.write_owned_vectored ~bufs writer with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error (`Net Connection_closed | `Process_down | `Timeout) -> close ()
        | Error (`Net err) ->
            Logger.error (fun f -> f "error: %a" Gluon.pp_err err);
            close ())
    | Error (`Process_down | `Timeout) ->
        Logger.error (fun f -> f "error: process down / timed out");
        close ()
    | Error (`Net err) ->
        Logger.error (fun f -> f "error: %a" Gluon.pp_err err);
        close ()
  in

  echo ()

let client server_port main =
  let addr = Net.Addr.(tcp loopback server_port) in
  let conn = Net.Tcp_stream.connect addr |> Result.get_ok in
  Logger.debug (fun f -> f "Connected to server on %d" server_port);

  let reader = Net.Tcp_stream.to_reader conn in
  let writer = Net.Tcp_stream.to_writer conn in

  let rec send_loop n bufs =
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match IO.write_owned_vectored ~bufs writer with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error (`Net Connection_closed | `Process_down | `Timeout) ->
          Logger.debug (fun f -> f "connection closed")
      | Error (`Net (Unix_error { reason = ENOTCONN | EPIPE; _ })) ->
          send_loop n bufs
      | Error (`Net err) ->
          Logger.error (fun f -> f "error: %a" Gluon.pp_err err);
          send_loop (n - 1) bufs
  in
  let bufs = IO.Iovec.from_string "hello " in
  send_loop 10_000 bufs;
  let bufs = IO.Iovec.from_string "world\r\n" in
  send_loop 10_000 bufs;

  let rec recv_loop data =
    let buf = IO.Bytes.with_capacity 1024 in
    match IO.read reader buf with
    | Ok bytes ->
        Logger.debug (fun f -> f "Client received %d bytes" bytes);
        let bytes = IO.Bytes.sub buf ~pos:0 ~len:bytes in
        let data = data ^ IO.Bytes.to_string bytes in
        if String.ends_with ~suffix:"\r\n" data then data else recv_loop data
    | Error (`Net Connection_closed | `Process_down | `Timeout) ->
        Logger.error (fun f -> f "Server closed the connection");
        data
    | Error (`Net err) ->
        Logger.error (fun f -> f "error: %a" Gluon.pp_err err);
        data
  in
  let data = recv_loop "" in

  if String.length data = 0 then send main (Received "empty paylaod")
  else send main (Received data)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let socket, port = Port_finder.next_open_port () in
  let main = self () in
  let server = spawn (fun () -> server port socket) in
  let client = spawn (fun () -> client port main) in
  monitor server;
  monitor client;
  match receive_any ~after:500_000L () with
  | Received "hello world\r\n" ->
      Logger.info (fun f -> f "net_reader_writer_test: OK");

      shutdown ()
  | Received other ->
      Logger.error (fun f -> f "net_reader_writer_test: bad payload: %S" other);
      sleep 1.;
      Stdlib.exit 1
  | Process.Messages.Monitor (Process_down pid) ->
      let who = if Pid.equal pid server then "server" else "client" in
      Logger.error (fun f ->
          f "net_test: %s(%a) died unexpectedly" who Pid.pp pid);
      sleep 1.;
      Stdlib.exit 1
  | _ ->
      Logger.error (fun f -> f "net_reader_writer_test: unexpected message");
      sleep 1.;
      Stdlib.exit 1
