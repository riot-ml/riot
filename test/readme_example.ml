[@@@warning "-8"]

(* $MDX part-begin=main *)
open Riot

type Message.t += Hello_world

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
        match receive () with
        | Hello_world ->
            Logger.info (fun f -> f "hello world from %a!" Pid.pp (self ()));
            shutdown ())
  in
  send pid Hello_world
(* $MDX part-end *)
