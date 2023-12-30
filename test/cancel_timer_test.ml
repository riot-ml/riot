[@@@warning "-8"]

open Riot

type Message.t += A | B

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in

  (* We cancel the second timer before it sends any messages, so the only
     message in the inbox should be A (sent from the first timer) *)
  let (Ok _timer) = Timer.send_after this A ~after:20L in
  let (Ok timer) = Timer.send_interval this B ~every:50L in
  Timer.cancel timer;
  let message = receive ~after:10_000L () in
  let _ =
    match message with
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
