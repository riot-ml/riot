let rec loop ~recv name count =
  match recv () with
  | Some `kill -> Printf.printf "proc[%s]: dead at %d\n%!" name count
  | Some (`system (Riot.Monitor_process_died pid)) ->
      Printf.printf "proc[%s]: oh no %d died!\n%!" name pid
  | None -> loop ~recv name (count + 1)

let main () =
  let pids =
    List.init 1_000_000 (fun i ->
        Riot.spawn (fun ~recv ->
            let pid = "pid" ^ string_of_int i in
            loop ~recv pid 0))
  in
  Printf.printf "spawned %d processes\n%!" (List.length pids);

  let pid1 = List.nth pids (Random.int 230) in
  let pid2 = List.nth pids (Random.int 230) in
  let pid3 = List.nth pids (Random.int 230) in
  let pid4 = List.nth pids (Random.int 230) in

  Riot.monitor pid1 pid2;
  Riot.monitor pid2 pid3;
  Riot.monitor pid3 pid4;

  Riot.send pid4 `kill;

  ()

let () = Riot.run @@ main
