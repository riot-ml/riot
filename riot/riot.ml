include Lib

open Logger.Make (struct
  let namespace = [ "riot" ]
end)

let shutdown ?(status = 0) () =
  debug (fun f -> f "RIOT IS SHUTTING DOWN!");
  let pool = _get_pool () in
  Scheduler.Pool.shutdown pool status

let run ?(rnd = Random.State.make_self_init ()) ?workers main =
  let max_workers = Int.max 0 (Stdlib.Domain.recommended_domain_count () - 2) in
  let workers =
    match workers with Some w -> Int.min w max_workers | None -> max_workers
  in

  Log.debug (fun f -> f "Initializing Riot runtime...");
  Printexc.record_backtrace true;
  Core.Pid.reset ();
  Scheduler.Uid.reset ();

  let sch0 = Scheduler.make ~rnd () in
  let pool, domains = Scheduler.Pool.make ~main:sch0 ~domains:workers () in

  Scheduler.set_current_scheduler sch0;
  Scheduler.Pool.set_pool pool;

  let _pid = _spawn ~pool ~scheduler:sch0 main in
  Scheduler.run pool sch0 ();

  Log.debug (fun f -> f "Riot runtime shutting down...");
  List.iter Stdlib.Domain.join domains;
  Log.debug (fun f -> f "Riot runtime shutdown");
  Stdlib.exit pool.status

let start ?rnd ?workers ~apps () =
  run ?rnd ?workers @@ fun () ->
  let child_specs =
    List.map
      (fun (module App : Application.Intf) ->
        Supervisor.child_spec App.start ())
      apps
  in
  Supervisor.(
    start_supervisor
      {
        strategy = One_for_one;
        restart_limit = 1;
        restart_period = 0;
        child_specs;
        children = [];
        restarts = [];
      })
