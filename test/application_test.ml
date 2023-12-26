open Riot

module Test = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Debug);
    let pid =
      spawn (fun () ->
          Logger.info (fun f -> f "application_test: OK");
          sleep 0.5;
          shutdown ())
    in
    Ok pid
end

let () =
  Riot.start
    ~apps:[ (module Riot.Telemetry); (module Riot.Logger); (module Test) ]
    ()
