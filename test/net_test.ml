open Riot

type Message.t += Received of string

(* rudimentary tcp echo server *)
let server port socket =
  Logger.debug (fun f -> f "Started server on %d" port);
  process_flag (Trap_exit true);
  let conn, addr = Net.Socket.accept socket |> Result.get_ok in
  Logger.debug (fun f ->
      f "Accepted client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn);
  let close () =
    Net.Socket.close conn;
    Logger.debug (fun f ->
        f "Closed client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn)
  in

  let buf = IO.Buffer.with_capacity 1024 in
  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match Net.Socket.receive conn ~buf with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let data = IO.Buffer.sub ~off:0 ~len buf in
        match Net.Socket.send ~data conn with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error (`Closed | `Process_down | `Timeout) -> close ()
        | Error (`Unix_error unix_err) ->
            Logger.error (fun f ->
                f "send unix error %s" (Unix.error_message unix_err));
            close ())
    | Error (`Closed | `Timeout | `Process_down) -> close ()
    | Error (`Unix_error unix_err) ->
        Logger.error (fun f ->
            f "recv unix error %s" (Unix.error_message unix_err));
        close ()
  in
  echo ()

let client port main =
  let addr = Net.Addr.(tcp loopback port) in
  let conn = Net.Socket.connect addr |> Result.get_ok in
  Logger.debug (fun f -> f "Connected to server on %d" port);
  let data = IO.Buffer.of_string "hello world" in
  let rec send_loop n =
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match Net.Socket.send ~data conn with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error `Closed -> Logger.debug (fun f -> f "connection closed")
      | Error (`Process_down | `Timeout) -> Logger.debug (fun f -> f "timeout")
      | Error (`Unix_error (ENOTCONN | EPIPE)) -> send_loop n
      | Error (`Unix_error unix_err) ->
          Logger.error (fun f ->
              f "client unix error %s" (Unix.error_message unix_err));
          send_loop (n - 1)
  in
  send_loop 10_000;

  let buf = IO.Buffer.with_capacity 128 in
  let recv_loop () =
    match Net.Socket.receive ~buf conn with
    | Ok bytes ->
        Logger.debug (fun f -> f "Client received %d bytes" bytes);
        bytes
    | Error (`Closed | `Timeout | `Process_down) ->
        Logger.error (fun f -> f "Server closed the connection");
        0
    | Error (`Unix_error unix_err) ->
        Logger.error (fun f ->
            f "client unix error %s" (Unix.error_message unix_err));
        0
  in
  let len = recv_loop () in

  if len = 0 then send main (Received "empty paylaod")
  else send main (Received (IO.Buffer.to_string buf))

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
  match receive ~after:100_000L () with
  | exception Receive_timeout ->
      Logger.error (fun f -> f "net_test: test timed out");

      Stdlib.exit 1
  | Received "hello world" ->
      Logger.info (fun f -> f "net_test: OK");

      shutdown ()
  | Received other ->
      Logger.error (fun f -> f "net_test: bad payload: %S" other);

      Stdlib.exit 1
  | Process.Messages.Monitor (Process_down pid) ->
      let who = if Pid.equal pid server then "server" else "client" in
      Logger.error (fun f ->
          f "net_test: %s(%a) died unexpectedly" who Pid.pp pid);

      Stdlib.exit 1
  | _ ->
      Logger.error (fun f -> f "net_test: unexpected message");

      Stdlib.exit 1
