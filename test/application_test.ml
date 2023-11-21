open Riot

module Test = struct
  let name = "test"

  let start () =
    let pid =
      spawn (fun () ->
          Logger.info (fun f -> f "app started!");
          sleep 0.5;
          shutdown ())
    in
    Ok pid
end

let () =
  Riot.start
    ~apps:[ (module Riot.Telemetry); (module Riot.Logger); (module Test) ]
    ()
