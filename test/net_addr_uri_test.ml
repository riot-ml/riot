open Riot

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let addr =
    Net.Addr.of_uri (Uri.of_string "http://ocaml.org") |> Result.get_ok
  in
  Logger.debug (fun f -> f "got addr: %a" Net.Addr.pp addr);
  match Net.Tcp_stream.connect addr with
  | Ok _ | Error (`Net Connection_closed | `Process_down | `Timeout) ->
      Logger.info (fun f -> f "net_addr_uri_test: OK");

      shutdown ()
  | Error (`Net err) ->
      Logger.error (fun f -> f "net_addr_uri_test: %a" Gluon.pp_err err);
      Stdlib.exit 1

let () = Riot.run @@ main
