open Riot

module Registry_test = struct
  let name = "test"

  type Message.t += Hello

  let test () =
    Logger.set_log_level (Some Info);
    let pid = self () in
    let pid_name = "my pid" in

    (* send to unregistered process raises *)
    (try send_by_name ~name:pid_name Hello with
    | Invalid_destination "my pid" ->
        Logger.info (fun f ->
            f "process_registration_test: unregistered send raises correctly")
    | Invalid_destination name2 ->
        Runtime.Log.error (fun f ->
            f "process_registration_test: invalid destination! %s" name2);
        Stdlib.exit 1);

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

    (* test sending a message by name to a registered process that died *)
    let pid2 = spawn (fun () -> sleep 0.1) in
    let pid2_name = "another-name" in
    register pid2_name pid2;
    (* wait at least the same amount as it will be alive *)
    sleep 0.5;
    (* send to unregistered process raises *)
    (match send_by_name ~name:pid2_name Hello with
    | exception Invalid_destination "another-name" ->
        Logger.info (fun f ->
            f "process_registration_test: dead send by name raises correctly")
    | _ ->
        Runtime.Log.error (fun f ->
            f
              "process_registration_test: send to dead process by name \
               should've raised!");
        Stdlib.exit 1);

    sleep 0.5;
    shutdown ()

  let start () =
    let pid = spawn_link test in
    Ok pid
end

let () =
  Riot.start
    ~apps:
      [ (module Riot.Telemetry); (module Riot.Logger); (module Registry_test) ]
    ()
