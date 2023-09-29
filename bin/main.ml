open Riot

let rec loop ~recv name count =
  match recv () with
  | Some `kill -> Printf.printf "proc[%s]: dead at %d\n%!" name count
  | None -> loop ~recv name (count + 1)

let main () =
  let pids =
    List.init 100_000 (fun i ->
        Scheduler.spawn (fun ~recv ->
            let pid = "pid" ^ string_of_int i in
            loop ~recv pid 0))
  in
  Printf.printf "spawned %d processes\n%!" (List.length pids);

  Process.send (List.nth pids (Random.int 230)) `kill;
  Process.send (List.nth pids (Random.int 230)) `kill;
  Process.send (List.nth pids (Random.int 230)) `kill;
  Process.send (List.nth pids (Random.int 230)) `kill;
  Process.send (List.nth pids (Random.int 230)) `kill;
  Process.send (List.nth pids (Random.int 230)) `kill;
  ()

let () =
  Miou.run @@ fun () ->
  let scheduler = Scheduler.run () in
  Miou.await_all (Miou.call main :: scheduler) |> ignore
