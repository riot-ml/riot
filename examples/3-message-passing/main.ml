open Riot

type Message.t += Hello_world

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
        match[@warning "-8"] receive () with
        | Hello_world -> print_endline "Hello, World! :D")
  in
  send pid Hello_world
