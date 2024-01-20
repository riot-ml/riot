[@@@warning "-8"]

open Riot

exception Fail

let get_sid pid =
  match Seq.find (fun (p, _proc) -> Pid.equal pid p) (processes ()) with
  | Some (_pid, proc) -> Process.sid proc
  | None -> raise Not_found

let main () =
  let _ = Logger.start () |> Result.get_ok in
  (* Runtime.set_log_level (Some Debug); *)
  Logger.set_log_level (Some Debug);
  Runtime.Stats.start ~every:1_000_000L ();

  let _scheduler_hogger =
    spawn_pinned (fun () ->
        Logger.info (fun f -> f "hogger %a" Pid.pp (self ()));
        let i = ref 0 in
        while true do
          i := !i + 1;
          if !i mod 100000 = 0 then yield ()
        done)
  in

  let pid =
    spawn_pinned (fun () ->
        Logger.info (fun f -> f "pid %a" Pid.pp (self ()));
        let rec sleep_loop () =
          yield ();
          sleep_loop ()
        in
        sleep_loop ())
  in
  Logger.info (fun f -> f "spinning up processes");

  let last_sid = get_sid pid in
  let rec check_loop iters =
    if iters = 0 then (
      Logger.error (fun f ->
          f "process_stealing_test: process was not stolen by another scheduler");
      raise Fail);
    let current_sid = get_sid pid in
    if not (Core.Scheduler_uid.equal last_sid current_sid) then
      Logger.info (fun f -> f "process_stealing_test: OK")
    else check_loop (iters - 1)
  in
  check_loop 100000

let () = run ~workers:2 @@ main
