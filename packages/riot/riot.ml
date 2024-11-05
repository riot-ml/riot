include Riot_stdlib
open Riot_runtime
module Config = Config

open Logger.Make (struct
  let namespace = [ "riot" ]
end)

exception Riot_already_started

let shutdown ?(status = 0) () =
  debug (fun f -> f "RIOT IS SHUTTING DOWN!");
  let pool = _get_pool () in
  Scheduler.Pool.shutdown pool status

let started = ref false

let run ?(config = Config.default ()) main =
  if !started then raise Riot_already_started else started := true;

  let Config.{ workers; rnd; _ } = config in

  Log.debug (fun f -> f "Initializing Riot runtime...\n%a" Config.pp config);

  Printexc.record_backtrace true;
  Core.Pid.reset ();
  Scheduler.Uid.reset ();

  let sch0 = Scheduler.make ~rnd () in
  let pool, _domains = Scheduler.Pool.make ~main:sch0 ~domains:workers () in

  Scheduler.set_current_scheduler sch0;
  Scheduler.Pool.set_pool pool;

  let _pid = _spawn ~pool ~scheduler:sch0 main in
  Scheduler.run pool sch0 ();

  Log.debug (fun f -> f "Riot runtime shutdown");
  Stdlib.exit pool.status

let on_error (error : [ `Msg of string ]) =
  let backtrace = Printexc.get_backtrace () in
  let error_string =
    match error with `Msg reason -> Printf.sprintf "%s\n%s" reason backtrace
  in
  Log.error (fun f -> f "Riot raised an error: %s\n" error_string);
  1

let run_with_status ?config ~on_error main =
  run ?config @@ fun _ ->
  let status =
    match main () with Ok code -> code | Error reason -> on_error reason
  in
  shutdown ~status ()

let start ?(config = Config.default ()) ~apps () =
  run ~config @@ fun () ->
  let child_specs =
    List.map
      (fun (module App : Application.Intf) ->
        Supervisor.child_spec App.start ())
      apps
  in
  let restart_limit = config.supervisor_restart_limit in
  let restart_period = config.supervisor_restart_period in
  Supervisor.(
    start_supervisor
      {
        strategy = One_for_one;
        restart_limit;
        restart_period;
        child_specs;
        children = [];
        restarts = [];
      })
