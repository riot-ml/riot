[@@@warning "-8"]

open Riot

exception Fail

type Message.t += A | B

let main () =
  let (Ok _) = Logger.start () in
  Runtime.set_log_level (Some Info);
  let this = self () in

  let (Ok _) = Timer.send_after this A ~after:100L in
  let (Ok t) = Timer.send_after this B ~after:10L in
  Timer.cancel t;
  match receive ~after:10_000L () with
  | A ->
      Logger.debug (fun f ->
          f "cancel_timer_test: timer successfully cancelled");
      Logger.info (fun f -> f "cancel_timer_test: OK")
  | B ->
      Logger.error (fun f -> f "timer not cancelled");
      raise Fail
  | _ ->
      Logger.error (fun f -> f "no message sent");
      raise Fail

let () = Riot.run @@ main
