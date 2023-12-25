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
  let this = self () in

  let (Ok _timer) = Timer.send_after this A ~after:1.0 in
  let (Ok _timer) = Timer.send_after this C ~after:2.0 in
  let (Ok _timer) = Timer.send_after this D ~after:3.0 in
  let (Ok _timer) = Timer.send_after this E ~after:4.0 in
  send this B;

  let messages =
    [ receive (); receive (); receive (); receive (); receive () ] |> List.rev
  in

  let _ =
    match messages with
    | [ B; A; C; D; E ] ->
        Logger.debug (fun f ->
            f "send_after_test: messages respected send_after time")
    | _ ->
        let messages = messages |> List.map msg_to_str |> String.concat "," in
        Runtime.Log.error (fun f -> f "bad message sequence: %s" messages);
        assert false
  in

  Logger.info (fun f -> f "send_after_test: OK");
  sleep 0.01;
  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
