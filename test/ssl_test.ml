open Riot

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

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

  let certificates =
    let crt =
      let buf = IO.Buffer.with_capacity 4_096 in
      let _len =
        File.open_read "fixtures/tls.crt"
        |> File.to_reader |> IO.read_to_end ~buf |> Result.get_ok
      in
      let cs = Cstruct.of_bytes (IO.Buffer.to_bytes buf) in
      X509.Certificate.decode_pem_multiple cs |> Result.get_ok
    in
    let pk =
      let buf = IO.Buffer.with_capacity 4_096 in
      let file = File.open_read "fixtures/tls.key" in
      let reader = File.to_reader file in
      assert (Result.is_ok (IO.read_to_end ~buf reader));
      let data = IO.Buffer.to_bytes buf in
      let cs = Cstruct.of_bytes data in
      X509.Private_key.decode_pem cs |> Result.get_ok
    in
    `Single (crt, pk)
  in
  let config = Tls.Config.server ~certificates () in
  let ssl = SSL.of_server_socket ~config conn in
  let reader, writer = SSL.(to_reader ssl, to_writer ssl) in

  let buf = IO.Bytes.with_capacity 1024 in
  let rec echo () =
    Logger.debug (fun f ->
        f "Reading from client client %a (%a)" Net.Addr.pp addr Net.Socket.pp
          conn);
    match IO.read reader buf with
    | Ok len -> (
        Logger.debug (fun f -> f "Server received %d bytes" len);
        let bufs = IO.Iovec.(of_bytes buf |> sub ~len) in
        match IO.write_owned_vectored ~bufs writer with
        | Ok bytes ->
            Logger.debug (fun f -> f "Server sent %d bytes" bytes);
            echo ()
        | Error (`Closed | `Timeout | `Process_down) -> close ()
        | Error err ->
            Logger.error (fun f -> f "error %a" IO.pp_err err);
            close ())
    | Error err ->
        Logger.error (fun f -> f "error %a" IO.pp_err err);
        close ()
  in
  echo ()

let client server_port main =
  let addr = Net.Addr.(tcp loopback server_port) in
  let conn = Net.Tcp_stream.connect addr |> Result.get_ok in
  Logger.debug (fun f -> f "Connected to server on %d" server_port);

  let host =
    let domain_name = Domain_name.of_string_exn "localhost" in
    Domain_name.host_exn domain_name
  in

  let null ?ip:_ ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  let ssl = SSL.of_client_socket ~host ~config conn in
  let reader, writer = SSL.(to_reader ssl, to_writer ssl) in

  let data = IO.Bytes.of_string "hello world" in
  let bufs = IO.Iovec.of_bytes data in
  let rec send_loop n =
    if n = 0 then Logger.error (fun f -> f "client retried too many times")
    else
      match IO.write_owned_vectored ~bufs writer with
      | Ok bytes -> Logger.debug (fun f -> f "Client sent %d bytes" bytes)
      | Error (`Timeout | `Process_down | `Closed) ->
          Logger.debug (fun f -> f "connection closed")
      | Error (`Unix_error (ENOTCONN | EPIPE)) -> send_loop n
      | Error err ->
          Logger.error (fun f -> f "error %a" IO.pp_err err);
          send_loop (n - 1)
  in
  send_loop 10_000;

  let buf = IO.Bytes.with_capacity 1024 in
  let recv_loop () =
    match IO.read reader buf with
    | Ok bytes ->
        Logger.debug (fun f -> f "Client received %d bytes" bytes);
        bytes
    | Error err ->
        Logger.error (fun f -> f "Error: %a" IO.pp_err err);
        0
  in
  let len = recv_loop () in
  let buf = IO.Bytes.sub buf ~pos:0 ~len in

  if len = 0 then send main (Received "empty paylaod")
  else send main (Received (IO.Bytes.to_string buf))

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
  | Received "hello world" -> Logger.info (fun f -> f "ssl_test: OK")
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
