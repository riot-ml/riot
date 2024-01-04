[@@@warning "-8"]

open Riot

exception Error

let main () =
  (* spawn process that should throw which should terminate main process *)
  let _pid = spawn_link (fun () -> raise Error) in
  ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
