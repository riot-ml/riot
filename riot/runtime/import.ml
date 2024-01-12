open Core

let trace_send = Scheduler.Tracer.trace_send
let trace_proc_run = Scheduler.Tracer.trace_proc_run
let _get_pool = Scheduler.Pool.get_pool
let _get_sch = Scheduler.get_current_scheduler

let _get_proc pid =
  let pool = _get_pool () in
  let proc = Proc_table.get pool.processes pid |> Option.get in
  proc

let self () = Scheduler.get_current_process_pid ()

let syscall name mode fd cb =
  Effect.perform (Proc_effect.Syscall { name; mode; fd });
  Process.set_ready_fds (_get_proc (self ())) [];
  cb fd

let receive ?after ?ref () =
  let timeout =
    match after with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Receive { ref; timeout })

let yield () = Effect.perform Proc_effect.Yield
let random () = (_get_sch ()).rnd

let sleep time =
  let now = Unix.gettimeofday () in
  let rec go finish =
    yield ();
    let now = Unix.gettimeofday () in
    if now > finish then () else go finish
  in
  go (now +. time)

let process_flag flag =
  let this = self () in
  let proc = _get_proc this in
  Log.trace (fun f -> f "Process %a: updating process flag" Pid.pp this);
  Process.set_flag proc flag

let exit pid reason =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      Log.debug (fun f -> f "%a exited by %a" Pid.pp proc.pid Pid.pp (self ()));
      Process.mark_as_exited proc reason
  | None -> ()

(* NOTE(leostera): to send a message, we will find the receiver process
   in the process table and queue at the back of their mailbox
*)
let send pid msg =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      Process.send_message proc msg;
      Scheduler.awake_process pool proc;
      Log.trace (fun f ->
          f "sent message from %a to %a" Pid.pp (self ()) Process.pp proc)
  | None ->
      Log.debug (fun f ->
          f "COULD NOT DELIVER message from %a to %a" Pid.pp (self ()) Pid.pp
            pid)

exception Invalid_destination of string

let send_by_name ~name msg =
  let pool = _get_pool () in
  match Proc_registry.find_pid pool.registry name with
  | Some pid -> send pid msg
  | None -> raise (Invalid_destination name)

exception Link_no_process of Pid.t

let _link (proc1 : Process.t) (proc2 : Process.t) =
  Process.add_link proc1 proc2.pid;
  Process.add_link proc2 proc1.pid

let link pid =
  let this = self () in
  Log.debug (fun f -> f "linking %a <-> %a" Pid.pp this Pid.pp pid);
  let pool = _get_pool () in
  let this_proc = _get_proc this in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      if Process.is_alive proc then _link this_proc proc
      else raise (Link_no_process pid)
  | None -> ()

let _spawn ?(do_link = false) (pool : Scheduler.pool) (scheduler : Scheduler.t)
    fn =
  let proc =
    Process.make scheduler.uid (fun () ->
        try
          fn ();
          Normal
        with exn ->
          Log.error (fun f ->
              f "Process %a died with unhandled exception %s:\n%s" Pid.pp
                (self ()) (Printexc.to_string exn)
                (Printexc.get_backtrace ()));

          Exception exn)
  in

  if do_link then (
    let this = self () in
    Log.debug (fun f -> f "linking %a <-> %a" Pid.pp this Pid.pp proc.pid);
    let this_proc = _get_proc this in
    _link this_proc proc);

  Scheduler.Pool.register_process pool scheduler proc;
  Scheduler.awake_process pool proc;
  proc.pid

let spawn fn =
  let pool = _get_pool () in
  let scheduler = Scheduler.get_random_scheduler pool in
  _spawn pool scheduler fn

let spawn_link fn =
  let pool = _get_pool () in
  let scheduler = Scheduler.get_random_scheduler pool in
  _spawn ~do_link:true pool scheduler fn

let monitor pid =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc -> Process.add_monitor proc (self ())
  | None -> ()

let demonitor pid =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc -> Process.remove_monitor proc (self ())
  | None -> ()

let register pid name =
  let pool = _get_pool () in
  Proc_registry.register pool.registry pid name

let unregister name =
  let pool = _get_pool () in
  Proc_registry.unregister pool.registry name

let where_is name =
  let pool = _get_pool () in
  Proc_registry.find_pid pool.registry name

let processes () =
  yield ();
  let pool = _get_pool () in
  Proc_table.processes pool.processes

let is_process_alive pid =
  yield ();
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc -> Process.is_alive proc
  | None -> false

let rec wait_pids pids =
  match pids with
  | [] -> ()
  | pid :: tail -> wait_pids (if is_process_alive pid then pids else tail)

module Timer = struct
  type timeout = Util.Timeout.t
  type timer = unit Symbol.t

  let _set_timer pid msg time mode =
    let sch = _get_sch () in
    let timer_ref =
      Scheduler.set_timer sch time mode (fun () -> send pid msg)
    in
    Ok timer_ref

  let send_after pid msg ~after:time = _set_timer pid msg time `one_off
  let send_interval pid msg ~every:time = _set_timer pid msg time `interval

  let cancel timer =
    let sch = _get_sch () in
    Scheduler.remove_timer sch timer
end
