[@@@warning "-8"]

open Riot

type Message.t += A | B

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in

  let (Ok _) = Timer.send_after this A ~after:100L in
  let (Ok t) = Timer.send_after this B ~after:10L in
  Timer.cancel t;
  match receive ~after:10_000L () with
  | A ->
      Logger.debug (fun f ->
          f "cancel_timer_test: timer successfully cancelled");
      Logger.info (fun f -> f "cancel_timer_test: OK");
      shutdown ()
  | B ->
      Logger.error (fun f -> f "timer not cancelled");
      sleep 0.1;
      assert false
  | _ ->
      Logger.error (fun f -> f "no message sent");
      sleep 0.1;
      assert false

let () = Riot.run @@ main
