let rec loop count =
  let _pid = Riot.self () in
  match count with
  | 3 ->
      (* Riot.Logs.log (fun f -> f "%a: dead at %d\n%!" Riot.Pid.pp pid count); *)
      ()
  | _ ->
      (* Riot.Logs.log (fun f -> f "%a: count=%d\n%!" Riot.Pid.pp pid count); *)
      loop (count + 1)

let main () =
  Riot.Logs.log (fun f -> f "starting app");
  let pids = List.init 1_000_000 (fun _i -> Riot.spawn (fun () -> loop 0)) in
  Riot.Logs.log (fun f -> f "spawned %d process" (List.length pids));
  ()

let () =
  Result.get_ok @@ Riot.run @@ main;
  Riot.Logs.info (fun f -> f "done!")
