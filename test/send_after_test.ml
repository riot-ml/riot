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

  let (Ok _timer) = Timer.send_after this A ~after:10L in
  let (Ok _timer) = Timer.send_after this C ~after:20L in
  let (Ok _timer) = Timer.send_after this D ~after:30L in
  let (Ok _timer) = Timer.send_after this E ~after:40L in
  send this B;

  let after = 10_000L in
  let messages =
    [
      receive ~after ();
      receive ~after ();
      receive ~after ();
      receive ~after ();
      receive ~after ();
    ]
    |> List.rev
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

  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
