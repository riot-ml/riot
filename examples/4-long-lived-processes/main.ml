open Riot

type Message.t += Hello of string

let () =
  Riot.run @@ fun () ->
  let rec loop () =
    (match receive () with
    | Hello name -> print_endline ("Hello, " ^ name ^ "! :D")
    | _ -> print_endline "Oh no, an unhandled message! D:");
    loop ()
  in
  let pid = spawn loop in
  send pid (Hello "Joe");
  send pid (Hello "Mike");
  send pid (Hello "Robert")
