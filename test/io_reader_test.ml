open Riot

let test_with_buffer capacity =
  let file = File.open_read "./fixtures/io_readv.txt" in
  let reader = File.to_reader file in
  let reader = IO.Reader.Buffered.of_reader ~capacity reader in

  let buf = IO.Buffer.with_capacity 8 in

  let op1 = IO.Reader.read reader ~buf in
  let str1 = IO.Buffer.to_string buf in
  Logger.debug (fun f -> f "read #1: %d bytes" (Result.get_ok op1));

  let op2 = IO.Reader.read reader ~buf in
  Logger.debug (fun f -> f "read #2: %d bytes" (Result.get_ok op2));
  let str2 = IO.Buffer.to_string buf in

  let op3 = IO.Reader.read reader ~buf in
  Logger.debug (fun f -> f "read #3: %d bytes" (Result.get_ok op3));
  let str3 = IO.Buffer.to_string buf in

  let final_str = str1 ^ str2 ^ str3 in
  if String.equal final_str "hello world\n" then
    Logger.info (fun f -> f "io_reader_test(%d): OK" capacity)
  else (
    Logger.error (fun f ->
        f "io_readv_test(%d): unexpected input  %S %S %S" capacity str1 str2
          str3);
    sleep 0.1;
    let exception Fail in
    raise Fail)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  (* smallest buffer that will work than the target and the source*)
  test_with_buffer 4;
  (* smallest than the target and the source*)
  test_with_buffer 7;
  (* larger than the target but smaller than the and smaller than the sourcesrc *)
  test_with_buffer 10;
  (* larger than the target and the source *)
  test_with_buffer 100;
  sleep 0.1;
  shutdown ()
