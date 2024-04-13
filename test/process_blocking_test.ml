[@@@warning "-8"]

open Riot

type Message.t += Hello_world

let factorial n =
  let rec aux n acc =
    match n with
    | 1 -> acc
    | x -> aux (n-1) (acc * x)
  in
  aux n 1

let busy_worker 

let () =
  Riot.run @@ fun () ->
    (* Runtime.set_log_level (Some Trace); *)
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
  send pid Hello_world;
  let a = factorial 30 in
  print_int a;
