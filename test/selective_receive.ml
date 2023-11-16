[@@@warning "-8"]

open Riot

type Message.t += A | B | C | Continue

let loop pid =
  send pid A;
  receive () |> ignore;
  send pid B;
  send pid C

let rec collect_messages ref count =
  if count = 0 then []
  else
    let msg = if count = 1 then receive () else receive ~ref () in
    Logger.debug (fun f -> f "received messgge: %s" (Marshal.to_string msg []));
    msg :: collect_messages ref (count - 1)

let main () =
  let (Ok _) = Logger.start () in
  let this = self () in
  let pid1 = spawn (fun () -> loop this) in

  sleep 0.01;
  let ref = Ref.make () in
  send pid1 Continue;

  let () =
    match collect_messages ref 3 with
    | [ B; C; A ] -> Logger.info (fun f -> f "messages arrived as expected")
    | _msgs -> Logger.error (fun f -> f "messages arrived out of order?")
  in
  sleep 0.01;
  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run ~workers:0 @@ main
