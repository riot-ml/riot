[@@@warning "-8"]

open Riot

let main () =
  let this = self () in
  let pid = spawn (fun () -> ()) in
  monitor this pid;

  match receive () with
  | Process.Messages.Monitor (Process_down pid2) when Pid.equal pid pid2 ->
      Runtime.Log.info (fun f -> f "add_monitor: was notified of process death");
      shutdown ()
  | _ ->
      Runtime.Log.info (fun f ->
          f "add_monitor: was NOT notified of process death");
      Stdlib.exit 1

let () =
  Runtime.Log.set_log_level (Some Info);
  Riot.run ~workers:0 @@ main
