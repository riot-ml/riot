[@@@warning "-8"]

open Riot

type Message.t += A | B

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in

  let (Ok _timer) = Timer.send_after this A ~after:0.0000001 in
  send this B;

  let B = receive () in
  let A = receive () in

  Logger.info (fun f -> f "messages respected send_after time");

  sleep 0.01;
  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
