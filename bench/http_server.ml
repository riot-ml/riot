open Riot

let ( let* ) = Result.bind

let rec stats () =
  let stat = Gc.stat () in
  Logger.info (fun f ->
      f
        "gc: live_words=%d live_blocks=%d free_words=%d free_blocks=%d \
         fragments=%d compactions=%d heap_chunks=%d heap_words=%d"
        stat.live_words stat.live_blocks stat.free_words stat.free_blocks
        stat.fragments stat.compactions stat.heap_chunks stat.heap_words);
  sleep 5.;
  stats ()

let data =
  {%b| "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!" |}
  |> Bytestring.to_iovec

let bufs = IO.Iovec.create ~size:1024 ()

let rec conn_loop conn () =
  let rec handle_request () =
    let* _req = Net.Tcp_stream.receive conn ~bufs in
    let* _written = Net.Tcp_stream.send conn ~bufs:data in
    handle_request ()
  in
  match handle_request () with
  | Ok _ -> conn_loop conn ()
  | Error _err -> Net.Tcp_stream.close conn

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);

  let port = 2113 in
  let socket = Net.Tcp_listener.bind ~port () |> Result.get_ok in
  Logger.debug (fun f -> f "Started server on %d" port);
  Process.flag (Trap_exit true);

  (* spawn stats |> ignore; *)
  let rec accept_loop () =
    let* conn, addr = Net.Tcp_listener.accept socket in
    Logger.debug (fun f ->
        f "Accepted client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn);
    spawn (conn_loop conn) |> ignore;
    accept_loop ()
  in
  let acceptor () =
    match accept_loop () with
    | Ok () -> ()
    | Error err ->
        Logger.error (fun f -> f "error: %a" IO.pp_err (Obj.magic err))
  in

  let _ = List.init 0 (fun _ -> spawn_link acceptor) in
  acceptor ()

let () = Riot.run @@ main
