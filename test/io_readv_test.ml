open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let fd = File.open_read "./fixtures/io_readv.txt" in
  let buf = IO.Buffer.with_capacity 8 in
  let len = IO.single_read (File.fd fd) ~buf |> Result.get_ok in
  let str = Cstruct.to_string ~off:0 ~len (IO.Buffer.as_cstruct buf) in
  match str with
  | "hello wo" ->
      Logger.info (fun f -> f "io_readv_test: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);
      sleep 0.1;
      Stdlib.exit 1
