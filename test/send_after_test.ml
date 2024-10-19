[@@@warning "-8"]

open Riot

type Message.t += A | B | C | D | E

let msg_to_str = function
  | A -> "a"
  | B -> "b"
  | C -> "c"
  | D -> "d"
  | E -> "e"
  | _ -> "other"

let main () =
  let (Ok _) = Logger.start () in
  (* Runtime.set_log_level (Some Trace); *)
  Logger.set_log_level (Some Info);
  let this = self () in

  let (Ok _timer) = Timer.send_after this A ~after:1_000L in
  let (Ok _timer) = Timer.send_after this C ~after:2_000L in
  let (Ok _timer) = Timer.send_after this D ~after:3_000L in
  let (Ok _timer) = Timer.send_after this E ~after:4_000L in
  send this B;

  let after = 10_000L in
  let messages =
    [
      receive_any ~after ();
      receive_any ~after ();
      receive_any ~after ();
      receive_any ~after ();
      receive_any ~after ();
    ]
    |> List.rev
  in

  match messages with
  | [ B; A; C; D; E ] ->
      Logger.debug (fun f ->
          f "send_after_test: messages respected send_after time");
      Logger.info (fun f -> f "send_after_test: OK")
  | _ ->
      let messages = messages |> List.map msg_to_str |> String.concat "," in
      Riot_runtime.Log.error (fun f -> f "bad message sequence: %s" messages);
      sleep 0.1;
      shutdown ~status:1 ()

let () = Riot.run ~config:(Config.make ~workers:0 ()) @@ main
