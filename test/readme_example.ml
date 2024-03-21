[@@@warning "-8"]

(* $MDX part-begin=main *)
open Riot

type Message.t += Hello_world

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
        let selector msg =
          match msg with Hello_world -> `select `hello_world | _ -> `skip
        in
        match receive ~selector () with
        | `hello_world ->
            Logger.info (fun f -> f "hello world from %a!" Pid.pp (self ()));
            shutdown ())
  in
  send pid Hello_world
(* $MDX part-end *)
