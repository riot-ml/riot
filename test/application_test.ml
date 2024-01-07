open Riot

module Test = struct
  let start () =
    Logger.set_log_level (Some Info);
    let pid =
      spawn (fun () ->
          Logger.info (fun f -> f "application_test: OK");

          shutdown ())
    in
    Ok pid
end

let () =
  Riot.start
    ~apps:[ (module Riot.Telemetry); (module Riot.Logger); (module Test) ]
    ()
