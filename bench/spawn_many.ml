open Riot

module Test_app = struct
  [@@@warning "-8"]

  type Riot.Message.t += Loop_stop

  let loop count =
    match receive_any () with
    | Loop_stop -> Log.debug (fun f -> f "dead at %d%!" count)

  let main t0 () =
    Logger.info (fun f -> f "boot test app");
    let pids =
      List.init 1_000_000 (fun _i ->
          spawn (fun () ->
              Logger.debug (fun f -> f "spawned %a" Pid.pp (self ()));
              loop 0))
    in
    Logger.info (fun f ->
        let t1 = Ptime_clock.now () in
        let delta = Ptime.diff t1 t0 in
        let delta = Ptime.Span.to_float_s delta in
        f "spawned %d processes in %.3fs" (List.length pids) delta);

    List.iter (fun pid -> send pid Loop_stop) pids;

    wait_pids pids;

    Logger.info (fun f ->
        let t1 = Ptime_clock.now () in
        let delta = Ptime.diff t1 t0 in
        let delta = Ptime.Span.to_float_s delta in
        f "spawned/awaited %d processes in %.3fs" (List.length pids) delta);
    sleep 0.001;
    shutdown ()

  let start () =
    Logger.set_log_level (Some Info);
    let t0 = Ptime_clock.now () in
    Ok (spawn_link (main t0))
end

let () = Riot.start ~apps:[ (module Logger); (module Test_app) ] ()
