open Riot

[@@@warning "-8"]
[@@@warning "-38"]

type Riot.Message.t += Loop_stop

let loop count =
  match receive () with
  | Loop_stop -> Logger.debug (fun f -> f "dead at %d%!" count)

let main t0 () =
  (* Riot.trace_proc_run  (fun sch proc -> 
    Format.printf "%2d %a\n%!" sch Process.pp proc
  ); *)

  let (Ok ()) = Logger.start ~print_source:true () in

  let pids = List.init 1_000_000 (fun _i -> spawn (fun () -> loop 0)) in

  List.iter (fun pid -> send pid Loop_stop) pids;

  wait_pids pids;

  let t1 = Ptime_clock.now () in
  Logger.info (fun f ->
      let delta = Ptime.diff t1 t0 in
      let delta = Ptime.Span.to_float_s delta in
      f "spawned/awaited %d processes in %.3fs" (List.length pids) delta);
  sleep 0.001;
  shutdown ()

let () =
  let t0 = Ptime_clock.now () in
  Logger.set_log_level (Some Info);
  Riot.run @@ main t0
