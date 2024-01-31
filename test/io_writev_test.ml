open Riot

exception Fail

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let now = Ptime_clock.now () in
  let file =
    Format.asprintf "./generated/%a.io_writev.txt" (Ptime.pp_rfc3339 ()) now
  in

  (* write to the file *)
  let fd = File.open_write file in
  let writer = File.to_writer fd in
  let bufs = IO.Iovec.from_string {| this is some data |} in
  let len = IO.write_owned_vectored writer ~bufs |> Result.get_ok in
  File.close fd;

  (* read from the file *)
  let buf = IO.Bytes.with_capacity len in
  let fd = File.open_read file in
  let reader = File.to_reader fd in
  let len = IO.read reader buf |> Result.get_ok in
  File.close fd;
  match IO.Bytes.(sub ~pos:0 ~len buf |> to_string) with
  | {| this is some data |} ->
      File.remove file;
      Logger.info (fun f -> f "io_readv_test: OK")
  | str ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);

      raise Fail
