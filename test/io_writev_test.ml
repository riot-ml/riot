open Riot

let data = IO.Buffer.of_string {| this is some data |}

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let now = Ptime_clock.now () in
  let file =
    Format.asprintf "./generated/%a.io_writev.txt" (Ptime.pp_rfc3339 ()) now
  in
  let fd = File.open_write file in
  let len = IO.single_write (File.fd fd) ~data |> Result.get_ok in
  File.close fd;
  let buf = IO.Buffer.with_capacity len in
  let fd = File.open_read file in
  let len = IO.single_read (File.fd fd) ~buf |> Result.get_ok in
  File.close fd;
  match Cstruct.to_string ~off:0 ~len (IO.Buffer.as_cstruct buf) with
  | {| this is some data |} ->
      File.remove file;
      Logger.info (fun f -> f "io_readv_test: OK");
      sleep 0.1;
      shutdown ()
  | str ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);
      sleep 0.1;
      Stdlib.exit 1
