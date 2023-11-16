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
  Server.Logger.set_log_level (Some Info);

  (* now if we want to see our logs we shoulds tart our Logger
     note: there's only one logger process tree! even if we have multiple logger clients *)
  let _ = Riot.Logger.start () |> Result.get_ok in
  yield ();
  Logger.info (fun f -> f "Starting server on port %d" port);

  let (Ok _server) =
    Http_server.start_link ~port @@ fun reqd ->
    let open Httpaf in
    let body = "Hello World" in
    let headers =
      [ ("Content-Length", Int.to_string (String.length body)) ]
      |> Headers.of_list
    in
    let res = Response.create ~headers `OK in
    Reqd.respond_with_string reqd res body
  in

  receive () |> ignore

let () = Riot.run @@ main
