[@@@warning "-8"]

open Riot

type Message.t += A

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in

  let (Ok _timer) = Timer.send_interval this A ~every:50L in

  let A = receive () in
  let A = receive () in

  Logger.debug (fun f -> f "send_interval_test: messages sent with interval");
  Logger.info (fun f -> f "send_interval_test: OK");

  sleep 0.01;
  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
