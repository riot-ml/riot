Riot.run @@ fun () ->
let open Riot in
let pid = spawn (fun () -> Format.printf "Hello, %a!" Pid.pp (self ())) in
wait_pids [ pid ];
shutdown ()
