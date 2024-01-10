open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let now = Ptime_clock.now () in
  let path =
    Format.asprintf "./generated/%a.io_writer_test.txt" (Ptime.pp_rfc3339 ())
      now
  in
  let file = File.open_write path in
  let writer = File.to_writer file in
  let buf = IO.Bytes.of_string {| this is some data |} in
  let () = IO.write_all writer ~buf |> Result.get_ok in

  let file = File.open_read path in
  let reader = File.to_reader file in
  let buf = IO.Bytes.with_capacity 19 in
  let _read = IO.read reader ~buf in
  let str = IO.Bytes.to_string buf in

  match str with
  | {| this is some data |} ->
      File.remove path;
      Logger.info (fun f -> f "io_writer_test: OK");

      shutdown ()
  | str ->
      Logger.error (fun f -> f "io_writer_test: unexpected input %S" str);

      Stdlib.exit 1
