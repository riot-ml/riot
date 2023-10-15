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
  Socket.Logger.set_log_level (Some Info);

  (* now if we want to see our logs we shoulds tart our Logger
     note: there's only one logger process tree! even if we have multiple logger clients *)
  Riot.Logger.start ~print_time:true ~print_source:true () |> Result.get_ok;

  Logger.info (fun f -> f "Starting server on port %d" port);

  let (Ok _server) =
    Http_server.start_link ~port @@ fun reqd ->
    let body = "Hello World" in
    let headers =
      Httpaf.Headers.of_list
        [ ("Content-Length", Int.to_string (String.length body)) ]
    in
    let res = Httpaf.Response.create ~headers `OK in
    Httpaf.Reqd.respond_with_string reqd res body
  in

  receive () |> ignore

let () = Riot.run @@ main
