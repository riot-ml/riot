let trace_send = Tracer.trace_send
let trace_proc_run = Tracer.trace_proc_run
let _get_pool = Scheduler.Pool.get_pool
let yield () = Effect.perform Proc_effect.Yield
let self () = Scheduler.get_current_process_pid ()

let get_proc pid =
  let pool = _get_pool () in
  let proc = Proc_table.get pool.processes pid |> Option.get in
  proc

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
  let proc = get_proc this in
  Logs.trace (fun f -> f "Process %a: updating process flag" Pid.pp this);
  Process.set_flag proc flag

let exit pid reason =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      Logs.debug (fun f -> f "%a exited by %a" Pid.pp proc.pid Pid.pp (self ()));
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
      Logs.trace (fun f ->
          f "sent message from %a to %a" Pid.pp (self ()) Process.pp proc)
  | None -> Logs.debug (fun f -> f "COULD NOT DELIVER message to %a" Pid.pp pid)

exception Link_no_process of Pid.t

let _link (proc1 : Process.t) (proc2 : Process.t) =
  Process.add_link proc1 proc2.pid;
  Process.add_link proc2 proc1.pid

let link pid =
  let this = self () in
  Logs.debug (fun f -> f "linking %a <-> %a" Pid.pp this Pid.pp pid);
  let pool = _get_pool () in
  let this_proc = get_proc this in
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
          Logs.debug (fun f ->
              f "Process %a died with exception %s:\n%s" Pid.pp (self ())
                (Printexc.to_string exn)
                (Printexc.get_backtrace ()));
          Exception exn)
  in

  if do_link then (
    let this = self () in
    Logs.debug (fun f -> f "linking %a <-> %a" Pid.pp this Pid.pp proc.pid);
    let this_proc = get_proc this in
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

let monitor pid1 pid2 =
  let pool = _get_pool () in
  match Proc_table.get pool.processes pid2 with
  | Some proc -> Process.add_monitor proc pid1
  | None -> ()

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

let random () = (Scheduler.get_current_scheduler ()).rnd

let receive ?(select = fun _ -> Message.Take) () =
  Effect.perform (Proc_effect.Receive { select })
