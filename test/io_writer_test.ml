open Riot

let data = IO.Buffer.of_string {| this is some data |}

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
  let len = IO.Writer.write writer ~data |> Result.get_ok in

  let file = File.open_read path in
  let reader = File.to_reader file in
  let reader = IO.Reader.Buffered.of_reader reader in
  let buf = IO.Buffer.with_capacity len in
  let _read = IO.Reader.read reader ~buf in
  let str = IO.Buffer.to_string buf in

  match str with
  | {| this is some data |} ->
      File.remove path;
      Logger.info (fun f -> f "io_writer_test: OK");
      sleep 0.1;
      shutdown ()
  | str ->
      Logger.error (fun f -> f "io_writer_test: unexpected input %S" str);
      sleep 0.1;
      Stdlib.exit 1
