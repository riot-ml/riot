[@@@warning "-8"]

open Riot

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let this = self () in
  let pid = spawn (fun () -> ()) in
  monitor this pid;

  match receive ~after:500_000L () with
  | Process.Messages.Monitor (Process_down pid2) when Pid.equal pid pid2 ->
      Logger.debug (fun f -> f "add_monitor: was notified of process death");
      Logger.info (fun f -> f "add_monitor: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f -> f "add_monitor: was NOT notified of process death");
      Stdlib.exit 1

let () = Riot.run ~workers:0 @@ main
