open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let fd = File.open_read "./test/fixtures/io_readv.txt" in
  let buf = Cstruct.create 8 in
  let len = Riot.IO.single_read fd ~buf |> Result.get_ok in
  let str = Cstruct.to_string ~off:0 ~len buf in
  match str with
  | "hello wo" ->
      Logger.info (fun f -> f "io_readv_test: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);
      sleep 0.1;
      Stdlib.exit 1
