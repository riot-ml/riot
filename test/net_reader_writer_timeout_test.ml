open Riot

let server port socket =
  Logger.debug (fun f -> f "Started server on %d" port);
  let _conn, _addr = Net.Socket.accept socket |> Result.get_ok in
  Logger.debug (fun f -> f "accepted connection");
  receive () |> ignore;
  ()

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let socket, server_port = Port_finder.next_open_port () in
  let _server = spawn (fun () -> server server_port socket) in

  let addr = Net.Addr.(tcp loopback server_port) in
  let socket = Net.Socket.connect addr |> Result.get_ok in
  Logger.debug (fun f -> f "connected");

  let buf = IO.Bytes.with_capacity 10 in
  let reader = Net.Socket.to_reader ~timeout:10L socket in
  (match IO.read ~buf reader with
  | Error `Timeout -> Logger.debug (fun f -> f "receive timeout works")
  | Ok _ ->
      Logger.error (fun f -> f "receive timeout received something?");
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f -> f "receive timeout errored: %a" IO.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  let bufs = IO.Iovec.with_capacity 1024 in
  let writer = Net.Socket.to_writer ~timeout:10L socket in
  (match IO.write_owned_vectored ~bufs writer with
  | Error `Timeout -> Logger.debug (fun f -> f "send timeout works")
  | Ok _ ->
      Logger.error (fun f -> f "send timeout sent something?");
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f -> f "send timeout errored: %a" IO.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  Logger.info (fun f -> f "net_reader_writer_timeout: OK");
  shutdown ()
