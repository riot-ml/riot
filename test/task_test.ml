open Riot

exception Fail

let rec count_to x n =
  if x = n then n
  else (
    yield ();
    count_to (x + 1) n)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);

  let task = Task.async (fun () -> count_to 0 1_000) in

  match Task.await ~timeout:100_000L task with
  | Ok n ->
      Logger.debug (fun f -> f "task_test: finished with %d" n);
      Logger.info (fun f -> f "task_test: OK");
      shutdown ()
  | Error `Timeout ->
      Logger.error (fun f -> f "task_test: timeout");
      raise Fail
  | _ ->
      Logger.error (fun f -> f "net_test: unexpected message");
      raise Fail
