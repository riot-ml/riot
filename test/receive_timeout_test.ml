open Riot

type Message.t += Unexpected

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let _ = Timer.send_after (self ()) Unexpected ~after:100L |> Result.get_ok in

  match receive ~after:1L () with
  | exception Receive_timeout ->
      Logger.info (fun f -> f "receive_timeout_test: OK");

      shutdown ()
  | _ ->
      Logger.error (fun f -> f "receive_timeout_test: unexpected message");

      Stdlib.exit 1
