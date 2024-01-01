open Riot

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let addr = Net.Addr.of_uri (Uri.of_string "http://ocaml.org") |> Option.get in
  Logger.debug (fun f -> f "got addr: %a" Net.Addr.pp addr);
  match Net.Socket.connect addr with
  | Ok _ | Error `Closed ->
      Logger.info (fun f -> f "net_addr_uri_test: OK");

      shutdown ()
  | Error (`Unix_error err) ->
      Logger.error (fun f ->
          f "net_addr_uri_test: failed with: %s" (Unix.error_message err));

      Stdlib.exit 1

let () = Riot.run @@ main
