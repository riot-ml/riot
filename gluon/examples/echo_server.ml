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
    let* conn, _addr = Net.Tcp_listener.accept server in
    let token = Token.make (Atomic.fetch_and_add conn_id 1) in
    (* log "accepted %a with %a\n%!" Net.Addr.pp addr Token.pp token; *)
    let* () =
      Poll.register poll token
        Interest.(add readable writable)
        (Net.Tcp_stream.to_source conn)
    in
    Hashtbl.replace connections token conn;
    Ok ()
  in

  let read_write conn event token =
    if Event.is_readable event then
      (* log "event is readable \n%!"; *)
      let rec echo_loop src =
        (* log "echo loop\n%!"; *)
        let capacity = 1024 * 50 in
        let* data =
          Bytestring.with_bytes ~capacity (Net.Tcp_stream.read conn)
        in
        (* log "read %d into buffer\n%!" len; *)
        (* log "subbed buffer\n%!"; *)
        (* log "made %d bytestring \n%!" (Bytestring.length data); *)
        let* () =
          if Bytestring.length data = 0 then Error `Connection_closed else Ok ()
        in
        let* _written =
          Net.Tcp_stream.write_vectored conn (Bytestring.to_iovec data)
        in
        (* log "write %d bytes \n%!" written; *)
        let* () =
          Poll.reregister poll token Interest.(add readable writable) src
        in
        echo_loop src
      in
      let src = Net.Tcp_stream.to_source conn in
      match echo_loop src with
      | Ok _ | Error `Would_block -> `continue
      | Error _ -> `finished
    else (* log "event is not writable/readable? \n%!"; *)
      `continue
  in

  let handle_client event token =
    (* log "handling client event with %a\n%!" Token.pp token; *)
    match Hashtbl.find_opt connections token with
    | None ->
        (* log "no connection found for token %a\n%!" Token.pp token; *)
        Ok ()
    | Some conn ->
        (* log "found connection %a\n%!" Net.Tcp_stream.pp conn; *)
        if read_write conn event token = `finished then (
          (* log "deregistering connection %a\n%!" Net.Tcp_stream.pp conn; *)
          Hashtbl.remove connections token;
          Poll.deregister poll (Net.Tcp_stream.to_source conn))
        else Ok ()
  in

  let handle_event (event : Event.t) =
    let token = Event.token event in
    (* log "handling event with %a\n%!" Token.pp token; *)
    log "(server_token=%a)\n%!" Token.pp server_token;
    if Token.equal token server_token then accept ()
    else handle_client event token
  in

  let rec poll_loop () =
    let* events = Poll.poll ~max_events:100 poll in
    if List.length events > 0 then
      (* log "polled %d events\n%!" (List.length events); *)
      List.iter (fun e -> handle_event e |> handle_error) events;
    poll_loop ()
  in

  (* log "polling...\n%!"; *)
  poll_loop ()

let () = handle_error (run ())
