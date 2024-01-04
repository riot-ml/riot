open Riot

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

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

  let certificates =
    let crt =
      let buf = IO.Buffer.with_capacity 4_096 in
      let _len =
        File.open_read "fixtures/tls.crt"
        |> File.to_reader |> IO.Reader.read ~buf |> Result.get_ok
      in
      X509.Certificate.decode_pem_multiple (IO.Buffer.as_cstruct buf)
      |> Result.get_ok
    in
    let pk =
      let buf = IO.Buffer.with_capacity 4_096 in
      let _len =
        File.open_read "fixtures/tls.key"
        |> File.to_reader |> IO.Reader.read ~buf |> Result.get_ok
      in
      X509.Private_key.decode_pem (IO.Buffer.as_cstruct buf) |> Result.get_ok
    in
    `Single (crt, pk)
  in
  let config = Tls.Config.server ~certificates () in
  let ssl = SSL.of_server_socket ~config conn in
  let reader, writer = SSL.(to_reader ssl, to_writer ssl) in

  let buf = IO.Buffer.with_capacity 1024 in

  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match IO.Reader.read reader ~buf with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let data = IO.Buffer.sub ~off:0 ~len buf in
        match IO.write_all ~data writer with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error (`Closed | `Timeout | `Process_down) -> close ()
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

  let host =
    let domain_name = Domain_name.of_string_exn "localhost" in
    Domain_name.host_exn domain_name
  in

  let null ?ip:_ ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  let ssl = SSL.of_client_socket ~host ~config conn in
  let reader, writer = SSL.(to_reader ssl, to_writer ssl) in

  let data = IO.Buffer.of_string "hello world" in
  let rec send_loop n =
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match IO.write_all ~data writer with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error (`Timeout | `Process_down | `Closed) ->
          Logger.debug (fun f -> f "connection closed")
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
  let server =
    spawn (fun () ->
        try server port socket
        with SSL.Tls_failure failure ->
          Logger.error (fun f ->
              f "server error: %a" Tls.Engine.pp_failure failure))
  in
  let client =
    spawn (fun () ->
        try client port main
        with SSL.Tls_failure failure ->
          Logger.error (fun f ->
              f "client error: %a" Tls.Engine.pp_failure failure))
  in
  monitor server;
  monitor client;
  match receive ~after:500_000L () with
  | Received "hello world" ->
      Logger.info (fun f -> f "ssl_test: OK");

      shutdown ()
  | Received other ->
      Logger.error (fun f -> f "ssl_test: bad payload: %S" other);

      Stdlib.exit 1
  | Process.Messages.Monitor (Process_down pid) ->
      let who = if Pid.equal pid server then "server" else "client" in
      Logger.error (fun f ->
          f "ssl_test: %s(%a) died unexpectedly" who Pid.pp pid);

      Stdlib.exit 1
  | _ ->
      Logger.error (fun f -> f "ssl_test: unexpected message");

      Stdlib.exit 1
