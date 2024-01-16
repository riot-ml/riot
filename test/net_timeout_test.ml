open Riot

let server port socket =
  Logger.debug (fun f -> f "Started server on %d" port);
  process_flag (Trap_exit true);
  let _conn, _addr = Net.Tcp_listener.accept socket |> Result.get_ok in
  receive () |> ignore;
  ()

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let socket, server_port = Port_finder.next_open_port () in
  let _server = spawn (fun () -> server server_port socket) in

  let addr = Net.Addr.(tcp loopback server_port) in
  let socket = Net.Tcp_stream.connect addr |> Result.get_ok in

  let bufs = IO.Iovec.create ~size:12 () in
  (match Net.Tcp_stream.receive ~timeout:10L ~bufs socket with
  | exception Syscall_timeout ->
      Logger.debug (fun f -> f "receive timeout works")
  | Ok _ ->
      Logger.error (fun f -> f "receive timeout received something?");
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f -> f "receive timeout errored: %a" IO.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  (* NOTE(@leostera): sending small things is way faster than our minimum timer wheel ticks *)
  let bytes = Bytes.make (1_024 * 1_024 * 1_024) 'a' in
  let bufs = IO.Iovec.of_bytes bytes in
  (match Net.Tcp_stream.send ~timeout:10L ~bufs socket with
  | exception Receive_timeout -> Logger.debug (fun f -> f "send timeout works")
  | Ok len ->
      Logger.error (fun f -> f "send timeout sent something?: %d bytes" len);
      sleep 0.2;
      Stdlib.exit 1
  | Error err ->
      Logger.error (fun f -> f "send timeout errored: %a" IO.pp_err err);
      sleep 0.2;
      Stdlib.exit 1);

  Logger.info (fun f -> f "net_timeout_test: OK")
