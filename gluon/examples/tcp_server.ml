open Gluon

let ( let* ) = Result.bind
let log = Format.printf

let handle_error r =
  match r with
  | Ok x -> x
  | Error `Would_block -> Printf.sprintf "Would block\r\n%!" |> failwith
  | Error `No_info -> Printf.sprintf "No info\r\n%!" |> failwith
  | Error `Connection_closed ->
      Printf.sprintf "Connection closed\r\n%!" |> failwith
  | Error (`Exn exn) ->
      Printf.sprintf "Exn: %S\r\n%!" (Printexc.to_string exn) |> failwith
  | Error (`Unix_error err) ->
      Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err) |> failwith
  | Error _ -> Printf.sprintf "other error" |> failwith

let run () =
  let addr = Net.Addr.(tcp loopback 9001) in

  let* poll = Poll.make () in
  let* server = Net.Tcp_listener.bind addr in

  let server_token = Token.make 2112 in
  let* () =
    Poll.register poll server_token Interest.readable
      (Net.Tcp_listener.to_source server)
  in

  let connections : (Token.t, Net.Tcp_stream.t) Hashtbl.t =
    Hashtbl.create 1024
  in

  let conn_id = Atomic.make 3000 in
  let accept () =
    log "accepting connection\n%!";
    let* conn, addr = Net.Tcp_listener.accept server in
    let token = Token.make (Atomic.fetch_and_add conn_id 1) in
    log "accepted %a with %a\n%!" Net.Addr.pp addr Token.pp token;
    let* () =
      Poll.register poll token
        Interest.(add readable writable)
        (Net.Tcp_stream.to_source conn)
    in
    Hashtbl.replace connections token conn;
    Ok ()
  in

  let data = {%b| "Hello world!\n" |} in

  let read_write conn event token =
    log "enter read_write\n";
    if Event.is_writable event then (
      log "event is writable \n";
      let write_result =
        match Net.Tcp_stream.write_vectored conn (Bytestring.to_iovec data) with
        | Ok _ ->
            Poll.reregister poll token Interest.readable
              (Net.Tcp_stream.to_source conn)
        | Error `Would_block -> Ok ()
        | Error err -> Error err
      in
      if Result.is_ok write_result then `continue else `finished)
    else if Event.is_readable event then (
      log "event is readable \n";
      let rec read_loop data =
        let open Bytestring in
        let* read =
          with_bytes ~capacity:4096 (fun buf ->
              match Net.Tcp_stream.read conn buf with
              | Ok n -> Ok n
              | Error `Would_block -> Ok 0
              | Error err -> Error err)
        in
        let data = data ^ read in
        if length read = 0 then Ok data else read_loop data
      in
      let data = read_loop {%b||} |> handle_error in
      log "Received data: %S" (Bytestring.to_string data);
      `finished)
    else (
      log "event is not writable/readable? \n";
      `continue)
  in

  let handle_client event token =
    log "handling client event with %a\n%!" Token.pp token;
    match Hashtbl.find_opt connections token with
    | None ->
        log "no connection found for token %a\n%!" Token.pp token;
        Ok ()
    | Some conn ->
        log "found connection %a\n" Net.Tcp_stream.pp conn;
        if read_write conn event token = `finished then (
          Hashtbl.remove connections token;
          Poll.deregister poll (Net.Tcp_stream.to_source conn))
        else Ok ()
  in

  let handle_event (event : Event.t) =
    let token = Event.token event in
    log "handling event with %a\n%!" Token.pp token;
    log "(server_token=%a)\n%!" Token.pp server_token;
    if Token.equal token server_token then accept ()
    else handle_client event token
  in

  let rec poll_loop () =
    let* events = Poll.poll ~max_events:100 poll in
    if List.length events > 0 then
      log "polled %d events\n%!" (List.length events);
    List.iter (fun e -> handle_event e |> handle_error) events;
    poll_loop ()
  in

  log "polling...\n%!";
  poll_loop ()

let () = handle_error (run ())
