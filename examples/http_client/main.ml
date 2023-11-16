[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

let port = 2112

let main () =
  (*
  Riot.trace_proc_run (fun sch proc ->
      Format.printf "%2d %a\n%!" sch Process.pp proc);
      *)
  (* since we have several sub-loggers with prefixes, we will set them all to Debug *)
  Riot.Logger.set_log_level (Some Info);

  (* now if we want to see our logs we shoulds tart our Logger
     note: there's only one logger process tree! even if we have multiple logger clients *)
  let _ =
    Riot.Logger.start ~print_time:true ~print_source:true () |> Result.get_ok
  in
  yield ();
  Logger.info (fun f -> f "Fetching google.com");

  let uri = Uri.of_string "http://0.0.0.0:2112" in
  let (Some addr) = Net.Addr.of_uri uri in
  let _ =
    match Net.Socket.connect addr with
    | Ok _sock -> Logger.info (fun f -> f "connected!")
    | Error `Closed -> Logger.error (fun f -> f "connection closed")
    | Error (`Unix_error unix_err) ->
        Logger.error (fun f ->
            f "connection error: %s" (Unix.error_message unix_err))
  in

  receive () |> ignore

let () = Riot.run @@ main
