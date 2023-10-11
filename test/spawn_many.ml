open Riot

[@@@warning "-8"]
[@@@warning "-38"]

type Riot.Message.t += Loop_stop | Count

let rec loop count =
  let pid = self () in
  match receive () with
  | Loop_stop ->
      Logger.debug (fun f -> f "%a: dead at %d%!" Pid.pp pid count);
      ()
  | _ ->
      Logger.debug (fun f -> f "%a: count=%d%!" Pid.pp pid count);
      loop (count + 1)

let rec wait_pids pids =
  match pids with
  | [] -> ()
  | pid :: tail -> wait_pids (if is_process_alive pid then pids else tail)

let main t0 () =
  let (Ok ()) = Logger.start () in

  let pids =
    List.init 1_000_000 (fun _i ->
        let pid = spawn (fun () -> loop 0) in
        Logger.debug (fun f -> f "spawned %a" Pid.pp pid);
        pid)
  in

  List.iter (fun pid -> send pid Loop_stop) pids;

  wait_pids pids;

  let t1 = Ptime_clock.now () in
  Logger.info (fun f ->
      let delta = Ptime.diff t1 t0 in
      let delta = Ptime.Span.to_float_s delta in
      f "spawned/awaited %d processes in %.3fs" (List.length pids) delta);

  shutdown ()

let () =
  let t0 = Ptime_clock.now () in
  Logger.set_log_level (Some Info);
  Riot.run @@ main t0
