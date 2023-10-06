open Riot

[@@@warning "-38"]

type Riot.Message.t += Loop_stop | Count

let rec loop count =
  let pid = self () in
  match receive () with
  | Loop_stop ->
      Logs.info (fun f -> f "%a: dead at %d%!" Pid.pp pid count);
      ()
  | _ ->
      Logs.info (fun f -> f "%a: count=%d%!" Pid.pp pid count);
      loop (count + 1)

let rec wait_pids pids =
  match pids with
  | [] -> ()
  | pid :: tail -> wait_pids (if is_process_alive pid then pids else tail)

let main t0 () =
  Logs.log (fun f -> f "starting app");
  let pids =
    List.init 10_000 (fun _i ->
        let pid = spawn (fun () -> loop 0) in
        Logs.info (fun f -> f "spawned %a" Pid.pp pid);
        pid)
  in
  Logs.log (fun f -> f "spawned %d process" (List.length pids));

  List.iter (fun pid -> send pid Loop_stop) pids;
  Logs.log (fun f -> f "sent messages");

  Logs.log (fun f -> f "awaiting processes to stop");
  wait_pids pids;

  let t1 = Ptime_clock.now () in
  Logs.log (fun f ->
      let delta = Ptime.diff t1 t0 in
      let delta = Ptime.Span.to_float_s delta in
      f "\n\n spawned/awaited %d processes in %fs\n\n" (List.length pids) delta);

  shutdown ()

let () =
  let t0 = Ptime_clock.now () in
  Logs.set_log_level None;
  Riot.run @@ main t0
