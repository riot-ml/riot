open Riot

let main () =
  let this = self () in
  let pid = spawn (fun () -> ()) in
  monitor this pid;

  let open Riot.Message in
  match receive () with
  | Monitor (Process_down pid2) when Pid.equal pid pid2 ->
      Logs.log (fun f -> f "was notified of process death");
      shutdown ()
  | _ ->
      Logs.log (fun f -> f "was NOT notified of process death");
      Stdlib.exit 1

let () =
  Logs.set_log_level (Some Info);
  Riot.run @@ main
