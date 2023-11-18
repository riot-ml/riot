include Runtime
include Lib

let shutdown () =
  Logger.warn (fun f -> f "RIOT IS SHUTTING DOWN!");
  let pool = _get_pool () in
  Scheduler.Pool.shutdown pool

let run ?(rnd = Random.State.make_self_init ())
    ?(workers = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Printexc.record_backtrace true;
  Pid.reset ();
  Scheduler.Uid.reset ();

  let sch0 = Scheduler.make ~rnd () in
  let pool, domains = Scheduler.Pool.make ~main:sch0 ~domains:workers () in

  Scheduler.set_current_scheduler sch0;
  Scheduler.Pool.set_pool pool;

  let _pid = _spawn pool sch0 main in
  Scheduler.run pool sch0 ();

  Logs.debug (fun f -> f "Riot runtime shutting down...");
  List.iter Stdlib.Domain.join domains;
  Logs.debug (fun f -> f "Riot runtime shutdown")

let start ?rnd ?workers ~apps () =
  run ?rnd ?workers @@ fun () ->
  let pids =
    List.fold_left
      (fun acc (module App : Application.Intf) ->
        match (acc, App.start ()) with
        | Ok acc, Ok pid -> Ok (pid :: acc)
        | Ok _, Error error ->
            Logs.error (fun f ->
                f "Could not start application %s due to %s" App.name
                  (Marshal.to_string error []));
            Error ()
        | Error (), _ -> acc)
      (Ok []) apps
    |> Result.get_ok
  in
  wait_pids pids
