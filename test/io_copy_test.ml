open Riot

let size = 1024 * 1024 * 1024 * 1
let data = String.make size '\001'
let data = IO.Buffer.of_string data
let buf = IO.Buffer.with_capacity (1024 * 1024 * 1)

let test_copy () =
  Logger.info (fun f -> f "io_reader_test: start");
  let dev_null = File.open_write "/dev/null" |> File.to_writer in

  let reader = IO.Reader.of_buffer data in
  let n = IO.copy ~buf reader dev_null |> Result.get_ok in

  if n = size then Logger.info (fun f -> f "io_reader_test: OK")
  else (
    Logger.error (fun f -> f "io_readv_test: copied %d != %d" n size);
    sleep 0.1;
    let exception Fail in
    raise Fail)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  sleep 0.1;
  test_copy ();
  sleep 0.1;
  shutdown ()
