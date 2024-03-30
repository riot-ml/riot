[@@@warning "-8"]

open Riot

type Message.t += A

let main () =
  let (Ok _) = Logger.start () in
  (* Runtime.set_log_level (Some Trace); *)
  Logger.set_log_level (Some Info);
  let this = self () in

  let (Ok _timer) = Timer.send_interval this A ~every:50L in

  let A = receive_any ~after:2000L () in
  let A = receive_any ~after:2000L () in

  Logger.debug (fun f -> f "send_interval_test: messages sent with interval");
  Logger.info (fun f -> f "send_interval_test: OK");

  shutdown ()

let () = Riot.run @@ main
