open Riot

let ( let* ) = Result.bind
let data = {%b| "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!" |}

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Runtime.set_log_level (Some Info);
  Logger.set_log_level (Some Info);

  let port = 2113 in
  let socket = Net.Tcp_listener.bind ~port () |> Result.get_ok in
  Logger.debug (fun f -> f "Started server on %d" port);
  Process.flag (Trap_exit true);

  let rec accept_loop () =
    let* conn, addr = Net.Tcp_listener.accept socket in
    Logger.debug (fun f ->
        f "Accepted client %a (%a)" Net.Addr.pp addr Net.Socket.pp conn);

    let _conn =
      spawn @@ fun () ->
      let rec conn_loop () =
        let rec handle_request () =
          let bufs = IO.Iovec.create ~size:(1024 * 50) () in
          let* _req =
            Net.Tcp_stream.receive ~timeout:5_000_000_000L conn ~bufs
          in
          let* _written =
            Net.Tcp_stream.send ~timeout:5_000_000_000L conn
              ~bufs:(Bytestring.to_iovec data)
          in
          handle_request ()
        in
        match handle_request () with
        | Ok _ -> conn_loop ()
        | Error _err -> Net.Tcp_stream.close conn
      in
      conn_loop ()
    in
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
