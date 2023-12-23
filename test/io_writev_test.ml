open Riot

let data = 
  let str = {| this is some data |} in
  Cstruct.of_string ~off:0 ~len:(String.length str) str

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let now = Ptime_clock.now () in
  let file = (Format.asprintf "./test/generated/%a.io_writev.txt" (Ptime.pp_rfc3339 ()) now) in
  let fd = File.open_write file in
  let len = IO.single_write fd ~data |> Result.get_ok in
  File.close fd;
  let buf = Cstruct.create len in
  let fd = File.open_read file in
  let len = IO.single_read fd ~buf |> Result.get_ok in
  File.close fd;
  match (Cstruct.to_string ~off:0 ~len buf) with
  | " this is some data " ->
      File.remove file;
      Logger.info (fun f -> f "io_readv_test: OK");
      sleep 0.1;
      shutdown ()
  | str ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);
      sleep 0.1;
      Stdlib.exit 1
