open Riot

let test_with_buffer capacity =
  let file = File.open_read "./fixtures/ocaml_org.html" in
  let reader = File.to_reader file in
  let buf = IO.Bytes.with_capacity 57946 in

  let len = IO.read reader ~buf |> Result.get_ok in
  let str1 = IO.Bytes.(sub buf ~pos:0 ~len |> to_string) in
  Logger.debug (fun f -> f "read #1: %d bytes" len);

  let len = IO.read reader ~buf |> Result.get_ok in
  let str2 = IO.Bytes.(sub buf ~pos:0 ~len |> to_string) in
  Logger.debug (fun f -> f "read #2: %d bytes" len);

  let len = IO.read reader ~buf |> Result.get_ok in
  let str3 = IO.Bytes.(sub buf ~pos:0 ~len |> to_string) in
  Logger.debug (fun f -> f "read #3: %d bytes" len);

  File.close file;
  let final_str = str1 ^ str2 ^ str3 in
  if String.length final_str = 57946 then
    Logger.info (fun f -> f "io_reader_large_test(%d): OK" capacity)
  else (
    Logger.error (fun f ->
        f "io_reader_large_test(%d): %d unexpected input  %S %S %S"
          (String.length final_str) capacity str1 str2 str3);

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
