[@@@warning "-8"]

open Riot

type Riot.Message.t +=
  | A of int
  | End
  | Collected_messages of Riot.Message.t list

type state = { messages : Riot.Message.t list; main : Pid.t }

let rec loop state =
  match receive_any ~after:500_000L () with
  | End -> send state.main (Collected_messages (List.rev state.messages))
  | A _ as msg -> loop { state with messages = msg :: state.messages }
  | _ -> loop state

let main () =
  let (Ok _) = Logger.start () in

  let this = self () in
  let pid = spawn (fun () -> loop { messages = []; main = this }) in
  send pid (A 1);
  send pid (A 2);
  send pid (A 3);
  send pid End;

  match receive_any ~after:500_000L () with
  | Collected_messages [ A 1; A 2; A 3 ] ->
      Logger.debug (fun f -> f "send_order_test: received messages in order");
      Logger.info (fun f -> f "send_order_test: OK");

      shutdown ()
  | _ ->
      Logger.info (fun f -> f "send_order_test: received messages out of order");

      Stdlib.exit 1

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
