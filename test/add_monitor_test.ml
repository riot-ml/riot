open Riot

exception Fail

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Runtime.set_log_level None;
  Logger.set_log_level (Some Info);
  let pid = spawn (fun () -> ()) in
  monitor pid;

  match receive ~after:500_000L () with
  | Process.Messages.Monitor (Process_down pid2) when Pid.equal pid pid2 ->
      Logger.debug (fun f -> f "add_monitor: was notified of process death");
      Logger.info (fun f -> f "add_monitor: OK")
  | _ ->
      Logger.error (fun f -> f "add_monitor: was NOT notified of process death");
      raise Fail

let () = Riot.run ~workers:1 @@ main
