open Riot

exception Fail

let fail () =
  sleep 0.2;
  raise Fail

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

  let bufs = IO.Iovec.create ~size:1024 () in
  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match Net.Tcp_stream.receive conn ~bufs with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let bufs = IO.Iovec.sub ~len bufs in
        match Net.Tcp_stream.send conn ~bufs with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error (`Closed | `Process_down | `Timeout) -> close ()
        | Error err ->
            Logger.error (fun f -> f "error %a" IO.pp_err err);
            close ())
    | Error (`Closed | `Timeout | `Process_down) -> close ()
    | Error err ->
        Logger.error (fun f -> f "error %a" IO.pp_err err);
        close ()
  in
  echo ()

let client server_port main =
  let addr = Net.Addr.(tcp loopback server_port) in
  let conn = Net.Tcp_stream.connect addr |> Result.get_ok in
  Logger.debug (fun f -> f "Connected to server on %d" server_port);
  let bufs = IO.Iovec.from_string "hello world" in
  let rec send_loop n =
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match Net.Tcp_stream.send ~bufs conn with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error `Closed -> Logger.debug (fun f -> f "connection closed")
      | Error (`Process_down | `Timeout) -> Logger.debug (fun f -> f "timeout")
      | Error (`Unix_error (ENOTCONN | EPIPE)) -> send_loop n
      | Error err ->
          Logger.error (fun f -> f "error %a" IO.pp_err err);
          send_loop (n - 1)
  in
  send_loop 10_000;

  let bufs = IO.Iovec.create ~size:1024 () in
  let recv_loop () =
    match Net.Tcp_stream.receive ~bufs conn with
    | Ok bytes ->
        Logger.debug (fun f -> f "Client received %d bytes" bytes);
        bytes
    | Error (`Closed | `Timeout | `Process_down) ->
        Logger.error (fun f -> f "Server closed the connection");
        0
    | Error err ->
        Logger.error (fun f -> f "error %a" IO.pp_err err);
        0
  in
  let len = recv_loop () in
  let str = IO.Iovec.(sub bufs ~len |> into_string) in

  if len = 0 then send main (Received "empty paylaod")
  else send main (Received str)

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
  match receive_any ~after:10_000_000L () with
  | exception Receive_timeout ->
      Logger.error (fun f -> f "net_test: test timed out");
      fail ()
  | Received "hello world" ->
      Logger.info (fun f -> f "net_test: OK");
      sleep 0.1
  | Received other ->
      Logger.error (fun f -> f "net_test: bad payload: %S" other);
      fail ()
  | Process.Messages.Monitor (Process_down pid) ->
      let who = if Pid.equal pid server then "server" else "client" in
      Logger.error (fun f ->
          f "net_test: %s(%a) died unexpectedly" who Pid.pp pid);
      fail ()
  | _ ->
      Logger.error (fun f -> f "net_test: unexpected message");

      fail ()
