open Riot

module Http = struct

  module Acceptor = struct
    let rec accept_loop () = yield ();
      accept_loop ()

    let start_link () = 
      let pid = spawn_link (fun () -> accept_loop ()) in
      Ok pid

    let child_spec = Supervisor.child_spec ~start_link ()

    module Sup = struct
      type state = { acceptors: int }

      let start_link {acceptors} =
        let child_specs = List.init acceptors (fun _ -> child_spec) in
        Supervisor.start_link ~child_specs ()

      let child_spec ~acceptors =  
        Supervisor.child_spec ~start_link { acceptors }
    end
  end


  module Connector = struct
    let rec connect_loop () = yield (); connect_loop ()

    let start_link () =
      let pid = spawn_link (fun () -> connect_loop ()) in
      Ok pid

    let child_spec =
      Supervisor.child_spec ~start_link ()

    module Sup = struct
      type state = { connectors: int }

      let start_link {connectors} =
        let child_specs = List.init connectors (fun _ -> child_spec) in
        Supervisor.start_link ~child_specs ()

      let child_spec ~connectors =  
        Supervisor.child_spec ~start_link { connectors }
    end
  end

  let start_link ?(acceptors = 20) ?(connectors = 20) () =
    let child_specs = [
      Connector.Sup.child_spec ~connectors;
      Acceptor.Sup.child_spec ~acceptors
    ] in
    Supervisor.start_link ~child_specs ()
end

let main () =
  let _server = Http.start_link () in
  ()

let () =
  Logs.set_log_level (Some Info);
  Riot.run @@ main
