open Riot

module Registry_test = struct
  let name = "test"

  type Message.t += Hello

  let test () =
    Logger.set_log_level (Some Info);
    let pid = self () in
    let pid_name = "my pid" in

    (* register the process once *)
    register pid_name pid;

    send_by_name ~name:pid_name Hello;

    (match[@warning "-8"] receive () with
    | Hello ->
        Logger.info (fun f -> f "process_registration_test: send_by_name works"));

    (* try to register it again *)
    (try register pid_name pid with
    | Name_already_registered ("my pid", pid2) when Pid.equal pid pid2 ->
        Logger.info (fun f ->
            f "process_registration_test: double register disallowed")
    | Name_already_registered (name, pid2) ->
        Runtime.Log.error (fun f ->
            f "process_registration_test: double registered! %s <-> %a" name
              Pid.pp pid2);
        Stdlib.exit 1);

    (* unregister/register again *)
    unregister pid_name;
    register pid_name pid;
    Logger.info (fun f -> f "process_registration_test: unregistering works");

    sleep 0.5;
    shutdown ()

  let start () =
    let pid = spawn test in
    Ok pid
end

let () =
  Riot.start
    ~apps:
      [ (module Riot.Telemetry); (module Riot.Logger); (module Registry_test) ]
    ()
