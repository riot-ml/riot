open Riot
open IO

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () in
  Logger.set_log_level (Some Debug);
  let buf = Buffer.of_string "hello\r\n0\r\nworld" in
  match Buffer.split ~on:"\r\n" buf |> List.map Buffer.to_string with
  | [ "hello"; "0"; "world" ] ->
      Logger.info (fun f -> f "io_buffer_test: OK");
      shutdown ()
  | strs ->
      Logger.error (fun f ->
          f "io_buffer_test: bad split! got: %S" (String.concat ", " strs));
      sleep 0.1;
      Stdlib.exit 1
