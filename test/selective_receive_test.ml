[@@@warning "-8"]

open Riot

type Message.t += A | B | C | Continue

let loop pid =
  send pid A;
  receive ~after:500_000L () |> ignore;
  send pid B;
  send pid C

let main () =
  let (Ok _) = Logger.start () in
  Logger.set_log_level (Some Info);
  let this = self () in

  let pid1 = spawn (fun () -> loop this) in
  (* we will wait so the first message from the process gets sent *)
  sleep 0.1;

  let ref = Symbol.make () in
  send pid1 Continue;

  let m1 = receive ~ref ~after:50_000L () in
  let m2 = receive ~ref ~after:50_000L () in
  let m3 = receive ~after:50_000L () in

  match (m1, m2, m3) with
  | B, C, A ->
      Logger.info (fun f -> f "selective_receive: OK");

      shutdown ()
  | m1, m2, m3 ->
      Logger.error (fun f ->
          f "selective_receive: messages arrived out of order?\n%S\n%S\n%S"
            (Marshal.to_string m1 []) (Marshal.to_string m2 [])
            (Marshal.to_string m3 []));

      sleep 1.;
      Stdlib.exit 1

let () = Riot.run ~workers:0 @@ main
