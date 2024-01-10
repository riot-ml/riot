open Riot
open IO

exception Fail

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let fd = File.open_read "fixtures/io_readv.txt" in
  let reader = File.to_reader fd in
  let buf = Bytes.with_capacity 8 in
  let len = IO.read reader ~buf |> Result.get_ok in
  let str = Bytes.(sub ~pos:0 ~len buf |> to_string) in
  match str with
  | "hello wo" -> Logger.info (fun f -> f "io_readv_test: OK")
  | _ ->
      Logger.error (fun f -> f "io_readv_test: unexpected input %S" str);

      sleep 0.1;
      raise Fail
