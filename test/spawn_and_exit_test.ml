[@@@warning "-8"]

open Riot

let rec loop () =
  yield ();
  loop ()

let main () =
  let (Ok _) = Logger.start () in

  (* spin up and wait for 1 second before terminating *)
  let pid1 = spawn (fun () -> loop ()) in

  (* once we send this exit signal to pid1, and it dies, it should take pid2 down with it *)
  exit pid1 Normal;

  (* so we'll wait for both pids to be dead *)
  wait_pids [ pid1 ];

  Logger.info (fun f -> f "spawn_and_exit: OK");

  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
