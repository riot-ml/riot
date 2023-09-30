open Riot

let rec loop ~recv name count =
  match recv () with
  | Some `kill -> Printf.printf "proc[%s]: dead at %d\n%!" name count
  | Some (`system (Process.Monitor_process_died pid)) ->
      Printf.printf "proc[%s]: oh no %d died!\n%!" name pid
  | None -> loop ~recv name (count + 1)

let main () =
  let pids =
    List.init 1_000_000 (fun i ->
        Scheduler.spawn (fun ~recv ->
            let pid = "pid" ^ string_of_int i in
            loop ~recv pid 0))
  in
  Printf.printf "spawned %d processes\n%!" (List.length pids);

  let pid1 = List.nth pids (Random.int 230) in
  let pid2 = List.nth pids (Random.int 230) in
  let pid3 = List.nth pids (Random.int 230) in
  let pid4 = List.nth pids (Random.int 230) in

  Process.monitor pid1 pid2;
  Process.monitor pid2 pid3;
  Process.monitor pid3 pid4;

  Process.send pid4 `kill;

  ()

let () =
  Miou.run @@ fun () ->
  let scheduler = Scheduler.run () in
  Miou.await_all (Miou.call main :: scheduler) |> ignore
