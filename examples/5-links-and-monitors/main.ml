open Riot

let rec await_monitor_message () =
  match receive () with
  | Process.Messages.Monitor (Process_down pid) ->
      Format.printf "uh-oh! Process %a terminated\n%!" Pid.pp pid
  | _ -> await_monitor_message ()

let rec loop () =
  yield ();
  loop ()

let () =
  Riot.run @@ fun () ->
  (* monitor *)
  let pid1 = spawn loop in
  let pid2 =
    spawn (fun () ->
        monitor pid1;
        await_monitor_message ())
  in
  sleep 0.1;
  exit pid1 Normal;
  wait_pids [ pid1; pid2 ];

  (* link *)
  let pid3 = spawn loop in
  let pid4 =
    spawn (fun () ->
        link pid3;
        loop ())
  in
  sleep 0.2;
  exit pid3 Normal;
  wait_pids [ pid3; pid4 ];
  Format.printf "both processes (%a,%a) have terminated\n%!" Pid.pp pid3 Pid.pp
    pid4
