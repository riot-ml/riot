open Riot

type Message.t += Received of string

(* rudimentary tcp echo server *)
let server port =
  let socket = Net.Socket.listen ~port () |> Result.get_ok in
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

  let reader = Net.Socket.to_reader conn in
  let writer = Net.Socket.to_writer conn in

  let buf = IO.Buffer.with_capacity 1024 in
  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match IO.Reader.read reader ~buf with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let data = IO.Buffer.sub ~off:0 ~len buf in
        match IO.Writer.write ~data writer with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error `Closed -> close ()
        | Error (`Unix_error unix_err) ->
            Logger.error (fun f ->
                f "send unix error %s" (Unix.error_message unix_err));
            close ())
    | Error (`Eof | `Closed | `Timeout) -> close ()
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

  let reader = Net.Socket.to_reader conn in
  let writer = Net.Socket.to_writer conn in

  let rec send_loop n =
    sleep 0.001;
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match IO.Writer.write ~data writer with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error `Closed -> Logger.debug (fun f -> f "connection closed")
      | Error (`Unix_error (ENOTCONN | EPIPE)) -> send_loop n
      | Error (`Unix_error unix_err) ->
          Logger.error (fun f ->
              f "client unix error %s" (Unix.error_message unix_err));
          send_loop (n - 1)
  in
  send_loop 10_000;

  let buf = IO.Buffer.with_capacity 128 in
  let recv_loop () =
    match IO.Reader.read ~buf reader with
    | Ok bytes ->
        Logger.debug (fun f -> f "Client received %d bytes" bytes);
        bytes
    | Error (`Eof | `Closed | `Timeout) ->
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
  let port = 2112 in
  let main = self () in
  let server = spawn (fun () -> server port) in
  let client = spawn (fun () -> client port main) in
  monitor main server;
  monitor main client;
  match receive ~after:500_000L () with
  | Received "hello world" ->
      Logger.info (fun f -> f "net_reader_writer_test: OK");
      sleep 0.001;
      shutdown ()
  | Received other ->
      Logger.error (fun f -> f "net_reader_writer_test: bad payload: %S" other);
      sleep 0.001;
      Stdlib.exit 1
  | Process.Messages.Monitor (Process_down pid) ->
      let who = if Pid.equal pid server then "server" else "client" in
      Logger.error (fun f ->
          f "net_test: %s(%a) died unexpectedly" who Pid.pp pid);
      sleep 0.001;
      Stdlib.exit 1
  | _ ->
      Logger.error (fun f -> f "net_reader_writer_test: unexpected message");
      sleep 0.001;
      Stdlib.exit 1
