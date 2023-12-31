[@@@warning "-8"]

open Riot

type Message.t += A | B

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in

  let (Ok _) = Timer.send_after this A ~after:100L in
  let (Ok t) = Timer.send_after this B ~after:10L in
  Timer.cancel t;
  let _ =
    match receive ~after:10_000L () with
    | A ->
        Logger.debug (fun f ->
            f "cancel_timer_test: timer successfully cancelled")
    | B ->
        Runtime.Log.error (fun f -> f "timer not cancelled");
        assert false
    | _ ->
        Runtime.Log.error (fun f -> f "no message sent");
        assert false
  in

  Logger.info (fun f -> f "cancel_timer_test: OK");
  sleep 0.01;
  shutdown ()

let () = Riot.run @@ main
