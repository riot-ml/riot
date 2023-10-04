[@@@warning "-38"]

open Riot

type Riot.Message.t += Loop_stop

let select msg = match msg with Loop_stop -> Message.Take | _ -> Message.Skip

let rec loop count =
  let pid = self () in
  match receive ~select () with
  | Loop_stop ->
      Logs.log (fun f -> f "%a: dead at %d\n%!" Pid.pp pid count);
      ()
  | _ ->
      (* Logs.log (fun f -> f "%a: count=%d\n%!" Pid.pp pid count); *)
      loop (count + 1)

let main () =
  let t0= Ptime_clock.now () in
  Logs.log (fun f -> f "starting app");
  let pids =
    List.init 10_000 (fun _i ->
        let pid = spawn (fun () -> loop 0) in
        Logs.log (fun f -> f "spawned %a" Pid.pp pid);
        pid)
  in
  Logs.log (fun f -> f "spawned %d process" (List.length pids));
  List.iter (fun pid -> send pid Loop_stop) pids;
  Logs.log (fun f -> f "sent messages");
  let t1 = Ptime_clock.now () in
  Logs.log (fun f ->
    let delta = Ptime.diff t1 t0 in
    let delta = Ptime.Span.to_float_s delta in
    f "whole flow took %f ms" delta);

  ()

let () =
  Logs.set_log_level Logs.Info;
  Result.get_ok @@ Riot.run @@ main;
  Logs.info (fun f -> f "done!")
