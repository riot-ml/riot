open Riot

let say_hello () =
  Logger.info (fun f -> f "hello from process %a" Pid.pp (self ()))

let () =
  Riot.run @@ fun () ->
  Logger.start () |> Result.get_ok;

  say_hello ();

  let pid1 = spawn say_hello in

  wait_pids [ pid1 ];

  Logger.info (fun f -> f "%a has terminated" Pid.pp pid1)
