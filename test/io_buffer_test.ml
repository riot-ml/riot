open Riot
open IO

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () in
  Logger.set_log_level (Some Info);
  let _buf = Buffer.of_string "hello\r\n0\r\nworld\r\nagain" in
  let buf =
    Buffer.of_string ("F4240\r\n" ^ String.make 1_000 'a' ^ "\r\nbbb")
  in
  match Buffer.split ~max:2 ~on:"\r\n" buf |> List.map Buffer.to_string with
  | [ "F4240"; _; "bbb" ] ->
      Logger.info (fun f -> f "io_buffer_test: OK");
      shutdown ()
  | strs ->
      Logger.error (fun f ->
          f "io_buffer_test: bad split! got: %S" (String.concat ", " strs));
      sleep 0.2;
      Stdlib.exit 1
