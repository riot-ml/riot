open Riot

let test_with_buffer capacity =
  let file = File.open_read "./fixtures/io_readv.txt" in
  let reader = File.to_reader file in

  let buf = IO.Bytes.with_capacity 8 in

  let op1 = IO.read reader buf |> Result.get_ok in
  let str1 = IO.Bytes.(sub buf ~pos:0 ~len:op1 |> to_string) in
  Logger.debug (fun f -> f "read #1: %d bytes - %S" op1 str1);

  let op2 = IO.read reader buf |> Result.get_ok in
  let str2 = IO.Bytes.(sub buf ~pos:0 ~len:op2 |> to_string) in
  Logger.debug (fun f -> f "read #2: %d bytes - %S" op2 str2);

  let op3 = IO.read reader buf |> Result.get_ok in
  let str3 = IO.Bytes.(sub buf ~pos:0 ~len:op3 |> to_string) in
  Logger.debug (fun f -> f "read #3: %d bytes - %S" op3 str3);

  let final_str = str1 ^ str2 ^ str3 in
  if String.equal final_str "hello world\n" then
    Logger.info (fun f -> f "io_reader_test(%d): OK" capacity)
  else (
    Logger.error (fun f ->
        f "io_readv_test(%d): unexpected input  %S %S %S" capacity str1 str2
          str3);

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

  shutdown ()
