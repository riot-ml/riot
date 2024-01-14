open Riot

exception Fail

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Trace);
  let pid = spawn (fun () -> ()) in
  Process.monitor pid;

  match receive ~after:500_000L () with
  | Process.Messages.Monitor (Process_down pid2) when Pid.equal pid pid2 ->
      Logger.debug (fun f -> f "add_monitor: was notified of process death");
      Logger.info (fun f -> f "add_monitor: OK");
      sleep 0.2
  | (exception _) | _ ->
      Logger.error (fun f -> f "add_monitor: was NOT notified of process death");
      sleep 0.2;
      raise Fail

let () = Riot.run ~workers:1 @@ main
