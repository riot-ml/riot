open Core

let _get_pool = Scheduler.Pool.get_pool
let _get_sch = Scheduler.get_current_scheduler

let _get_proc pid =
  let pool = _get_pool () in
  let proc = Proc_table.get pool.processes pid |> Option.get in
  proc

let self () = Scheduler.get_current_process_pid ()

let syscall ?timeout name interest source cb =
  let timeout =
    match timeout with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout });
  cb ()

let receive :
    type msg.
    selector:(Message.t -> [ `select of msg | `skip ]) ->
    ?after:int64 ->
    ?ref:unit Ref.t ->
    unit ->
    msg =
 fun ~selector ?after ?ref () ->
  let timeout =
    match after with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Receive { ref; timeout; selector })

let receive_any ?after ?ref () =
  receive ~selector:(fun msg -> `select msg) ?after ?ref ()

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

let _spawn ?priority ?(do_link = false) ?(pool = _get_pool ())
    ?(scheduler = Scheduler.get_random_scheduler pool) fn =
  let proc =
    Process.make scheduler.uid (fun () ->
        try
          fn ();
          Normal
        with
        | Proc_state.Unwind -> Normal
        | exn ->
            Log.error (fun f ->
                f "Process %a died with unhandled exception %s:\n%s" Pid.pp
                  (self ()) (Printexc.to_string exn)
                  (Printexc.get_backtrace ()));

            Exception exn)
  in

  (match priority with
  | Some p -> Process.set_flag proc (Priority p)
  | None -> ());

  if do_link then (
    let this = self () in
    Log.debug (fun f -> f "linking %a <-> %a" Pid.pp this Pid.pp proc.pid);
    let this_proc = _get_proc this in
    _link this_proc proc);

  Scheduler.Pool.register_process pool proc;
  Scheduler.awake_process pool proc;
  proc.pid

let spawn fn = _spawn ~do_link:false fn

let spawn_pinned fn =
  _spawn ~do_link:false ~scheduler:(Scheduler.get_current_scheduler ()) fn

let spawn_link fn = _spawn ~do_link:true fn

let monitor pid =
  let pool = _get_pool () in
  let this = _get_proc (self ()) in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      Process.add_monitor proc this.pid;
      Process.add_monitored_by this proc.pid
  | None -> ()

let demonitor pid =
  let pool = _get_pool () in
  let this = _get_proc (self ()) in
  match Proc_table.get pool.processes pid with
  | Some proc ->
      Process.remove_monitor proc this.pid;
      Process.remove_monitored_by this proc.pid
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

let wait_pids pids =
  (* First we make sure we are monitoring all the pids we are awaiting *)
  List.iter monitor pids;

  (* Immediately after monitoring, we want to make sure we remove
     from the list all the pids that are already terminated, since we won't
     receive monitoring messages for those. *)
  let pool = _get_pool () in
  let pids =
    List.filter
      (fun pid ->
        match Proc_table.get pool.processes pid with
        | Some proc -> Process.is_alive proc
        | None -> false)
      pids
  in

  (* Now we can create our selector function to select the monitoring
     messages for the pids we care about. *)
  let selector msg =
    let open Process.Messages in
    match msg with
    | Monitor (Process_down pid) when List.mem pid pids ->
        `select (Process_down pid)
    | _ -> `skip
  in

  (* And we can enter the receive loop, filtering out the pids as they come.
     When the list of pids becomes empty, we exit the recursion. *)
  let rec do_wait pids =
    if List.length pids = 0 then ()
    else
      match receive ~selector () with
      | Process_down pid ->
          let pids = List.filter (fun pid' -> not (Pid.equal pid' pid)) pids in
          do_wait pids
  in
  do_wait pids

module Timer = struct
  type timeout = Util.Timeout.t
  type timer = unit Ref.t

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
