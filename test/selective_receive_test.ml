[@@@warning "-8"]

open Riot

type Message.t += A | B | C | Continue

let loop pid =
  send pid A;
  receive ~after:500_000L () |> ignore;
  send pid B;
  send pid C

let rec collect_messages ref count =
  if count = 0 then []
  else
    let msg =
      if count = 1 then receive ~after:500_000L ()
      else receive ~after:500_000L ~ref ()
    in
    Logger.debug (fun f -> f "received messgge: %s" (Marshal.to_string msg []));
    msg :: collect_messages ref (count - 1)

let main () =
  let (Ok _) = Logger.start () in
  Logger.set_log_level (Some Info);
  let this = self () in
  let pid1 = spawn (fun () -> loop this) in

  sleep 0.01;
  let ref = Ref.make () in
  send pid1 Continue;

  match collect_messages ref 3 with
  | [ B; C; A ] ->
      Logger.info (fun f -> f "selective_receive: OK");
      sleep 0.01;
      shutdown ()
  | _msgs ->
      Logger.error (fun f ->
          f "selective_receive: messages arrived out of order?");
      sleep 0.1;
      Stdlib.exit 1

let () = Riot.run ~workers:0 @@ main
