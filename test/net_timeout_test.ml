open Riot

let server port socket =
  Logger.debug (fun f -> f "Started server on %d" port);
  process_flag (Trap_exit true);
  let _conn, _addr = Net.Socket.accept socket |> Result.get_ok in
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

  let buf = IO.Buffer.with_capacity 10 in
  (match Net.Socket.receive ~timeout:10L ~buf socket with
  | Error `Timeout -> Logger.debug (fun f -> f "receive timeout works")
  | Ok _ ->
      Logger.error (fun f -> f "receive timeout received something?");
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f ->
          f "receive timeout errored: %a" Net.Socket.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  (match Net.Socket.send ~timeout:10L ~data:buf socket with
  | Error `Timeout -> Logger.debug (fun f -> f "send timeout works")
  | Ok _ ->
      Logger.error (fun f -> f "send timeout sent something?");
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f -> f "send timeout errored: %a" Net.Socket.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  Logger.info (fun f -> f "net_timeout_test: OK");
  shutdown ()
