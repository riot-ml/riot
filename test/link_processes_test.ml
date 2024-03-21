open Riot

let rec loop () =
  yield ();
  loop ()

let main () =
  let _ = Logger.start () |> Result.get_ok in
  (* Runtime.set_log_level (Some Debug); *)
  Logger.set_log_level (Some Info);

  Logger.debug (fun f -> f "spawning processes from %a" Pid.pp (self ()));

  (* spin up and wait for 1 second before terminating *)
  let pid1 =
    spawn (fun () ->
        Logger.debug (fun f -> f "spawned %a" Pid.pp (self ()));
        loop ())
  in

  (* spin up, link to pid1, and then loop infinitely *)
  let pid2 =
    spawn (fun () ->
        Logger.debug (fun f -> f "spawned %a" Pid.pp (self ()));
        link pid1;
        loop ())
  in

  let _ =
    spawn (fun () ->
        sleep 0.5;
        (* once we send this exit signal to pid1, and it dies, it should take pid2 down with it *)
        exit pid1 Normal)
  in

  (* so we'll wait for both pids to be dead *)
  wait_pids [ pid1; pid2 ];

  Logger.info (fun f -> f "link_procesess_test: OK")

let () = Riot.run @@ main
