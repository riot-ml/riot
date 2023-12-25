open Riot

let fail f =
  let exception Fail in
  Logger.error f;
  sleep 0.1;
  raise Fail

let size = 1024 * 1024 * 1024 * 1
let data = String.make size '7'
let data = IO.Buffer.of_string data

let test_copy () =
  Logger.debug (fun f ->
      f "io_copy_buffered_test: start (copying %d bytes)" size);
  let path = "./generated/super_large_file" in

  let dev_null = File.open_write path |> File.to_writer in
  let reader = IO.Reader.of_buffer data in
  let result = IO.copy_buffered reader dev_null in

  match result with
  | Ok n when n = size ->
      File.remove path;
      Logger.info (fun f -> f "io_copy_buffered_test: OK")
  | Ok n -> fail (fun f -> f "io_copy_buffered_test: copied %d != %d" n size)
  | Error `Closed -> fail (fun f -> f "io_copy_buffered_test: closed?")
  | Error `Eof -> fail (fun f -> f "io_copy_buffered_test: eof")
  | Error (`Unix_error err) ->
      fail (fun f ->
          f "io_copy_buffered_test: unix error: %s" (Unix.error_message err))

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  sleep 0.1;
  test_copy ();
  sleep 0.1;
  shutdown ()
