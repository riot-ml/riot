[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

module Logger = Logger.Make (struct
  let namespace = [ "http_server" ]
end)

let trace, info, debug, warn, error = Logger.(trace, info, debug, warn, error)

module type Connector = sig
  type Message.t += Handshake of Socket.connection

  val start_link : Socket.connection -> (Pid.t, [> `Connector_error ]) result
end

module type Protocol = sig
  val create_flow : unit -> Socket.Flow.t
end

module Tcp_connector (P : Protocol) : Connector = struct
  type Message.t += Handshake of Socket.connection

  let handle_handshake conn =
    debug (fun f -> f "Protocol handshake initiated");
    let flow = P.create_flow () in
    let _writer =
      spawn_link (fun () ->
          debug (fun f -> f "spawned writer %a" Pid.pp (self ()));
          Socket.Flow.write conn flow)
    in
    let _reader =
      spawn_link (fun () ->
          debug (fun f -> f "spawned reader %a" Pid.pp (self ()));
          Socket.Flow.read conn flow)
    in
    (* TODO(leostera): impl hibernate() this process should just sleep *)
    let rec loop () =
      yield ();
      loop ()
    in
    loop ()

  let start_link conn =
    let pid = spawn_link (fun () -> handle_handshake conn) in
    debug (fun f -> f "spawned tcp_conenctor %a" Pid.pp pid);
    Ok pid
end

module Acceptor = struct
  type state = {
    socket : Socket.t;
    open_connections : (Socket.connection * Pid.t) list;
    connector : (module Connector);
  }

  let rec accept_loop state =
    info (fun f -> f "Awaiting connection...");
    let conn = Socket.accept state.socket in
    let (module Connector) = state.connector in
    let (Ok pid) = Connector.start_link conn in
    debug (fun f -> f "Started connector at %a" Pid.pp pid);
    let state =
      { state with open_connections = (conn, pid) :: state.open_connections }
    in
    accept_loop state

  let start_link state =
    let pid = spawn_link (fun () -> accept_loop state) in
    Ok pid

  let child_spec ~socket (module C : Connector) =
    let state = { socket; open_connections = []; connector = (module C) } in
    Supervisor.child_spec ~start_link state

  module Sup = struct
    type state = {
      host : string;
      port : int;
      acceptors : int;
      connector : (module Connector);
    }

    let start_link { host; port; acceptors; connector } =
      let socket = Socket.listen ~host ~port ~max_requests:100 in
      let child_specs =
        List.init acceptors (fun _ -> child_spec ~socket connector)
      in
      Supervisor.start_link ~child_specs ()

    let child_spec ~host ~port ~acceptors ~connector =
      let state = { acceptors; host; port; connector } in
      Supervisor.child_spec ~start_link state
  end
end

let start_link ?(host = "0.0.0.0") ?(port = 2112) ?(acceptors = 20) connector ()
    =
  let child_specs =
    [ Acceptor.Sup.child_spec ~host ~port ~acceptors ~connector ]
  in
  Supervisor.start_link ~child_specs ()
