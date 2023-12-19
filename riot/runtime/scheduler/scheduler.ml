open Core
open Util
open Time
open Net
module Tracer = Tracer
module Uid = Scheduler_uid

type t = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  run_queue : Proc_queue.t;
  sleep_set : Proc_set.t;
  timers : Timer_wheel.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
}

type io = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  io_tbl : Io.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
}

type pool = {
  mutable stop : bool;
  io_scheduler : io;
  schedulers : t list;
  processes : Proc_table.t;
  registry : Proc_registry.t;
}

module Scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Log.debug (fun f -> f "Making scheduler with id: %a" Uid.pp uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      run_queue = Proc_queue.create ();
      sleep_set = Proc_set.create ();
      timers = Timer_wheel.create ();
      idle_mutex = Mutex.create ();
      idle_condition = Condition.create ();
    }

  let get_current_scheduler, (set_current_scheduler : t -> unit) =
    Thread_local.make ~name:"CURRENT_SCHEDULER"

  let get_current_process_pid, set_current_process_pid =
    Thread_local.make ~name:"CURRENT_PID"

  let get_random_scheduler : pool -> t =
   fun { schedulers = all_schedulers; _ } ->
    let sch = get_current_scheduler () in
    let rnd_idx = Random.State.int sch.rnd (List.length all_schedulers) in
    List.nth all_schedulers rnd_idx

  let set_timer sch time mode fn =
    Timer_wheel.make_timer sch.timers time mode fn

  let add_to_run_queue (sch : t) (proc : Process.t) =
    Mutex.protect sch.idle_mutex @@ fun () ->
    Proc_set.remove sch.sleep_set proc;
    Proc_queue.queue sch.run_queue proc;
    Condition.signal sch.idle_condition;
    Log.trace (fun f ->
        f "Adding process to run_queue queue[%d]: %a"
          (Proc_queue.size sch.run_queue)
          Pid.pp proc.pid)

  let awake_process pool (proc : Process.t) =
    List.iter
      (fun (sch : t) ->
        if Scheduler_uid.equal sch.uid proc.sid then add_to_run_queue sch proc)
      pool.schedulers

  let handle_receive k (proc : Process.t) (ref : unit Ref.t option) =
    let open Proc_state in
    Log.trace (fun f ->
        f "Process %a: receiving messages" Pid.pp (Process.pid proc));
    if Process.has_empty_mailbox proc then (
      Log.trace (fun f ->
          f "Process %a is awaiting for new messages" Pid.pp (Process.pid proc));
      Process.mark_as_awaiting_message proc;
      k Suspend)
    else
      let fuel = Process.message_count proc in
      Log.trace (fun f -> f "Skimming mailbox with %d messages" fuel);
      let rec go fuel =
        if fuel = 0 then k Delay
        else
          match (ref, Process.next_message proc) with
          | _, None ->
              Log.trace (fun f ->
                  f "Emptied the queue, will read from save queue next");
              Process.read_save_queue proc;
              k Delay
          | Some ref, Some msg when Ref.is_newer ref msg.uid ->
              Log.trace (fun f ->
                  f "Skipping msg ref=%a msg.uid=%a" Ref.pp ref Ref.pp msg.uid);
              Process.add_to_save_queue proc msg;
              go (fuel - 1)
          | _, Some Message.{ msg; _ } -> k (Continue msg)
      in
      go fuel

  let handle_syscall k pool (_sch : t) (proc : Process.t) syscall mode fd =
    let open Proc_state in
    Log.trace (fun f ->
        let mode = match mode with `r -> "r" | `w -> "w" | `rw -> "rw" in
        f "Registering %a for Syscall(%s,%s,%a)" Pid.pp proc.pid syscall mode
          Fd.pp fd);
    Io.register pool.io_scheduler.io_tbl proc mode fd;
    Process.mark_as_awaiting_io proc syscall mode fd;
    k Yield

  let perform pool (sch : t) (proc : Process.t) =
    let open Proc_state in
    let open Proc_effect in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      match eff with
      | Syscall { name; mode; fd } ->
          handle_syscall k pool sch proc name mode fd
      | Receive { ref } -> handle_receive k proc ref
      | Yield ->
          Log.trace (fun f ->
              f "Process %a: yielding" Pid.pp (Process.pid proc));
          k Yield
      | effect ->
          Log.trace (fun f ->
              f "Process %a: unhandled effect" Pid.pp (Process.pid proc));
          k (Reperform effect)
    in
    { perform }

  let handle_wait_proc _pool sch proc =
    if Process.has_messages proc then (
      Process.mark_as_runnable proc;
      Log.debug (fun f -> f "Waking up process %a" Pid.pp proc.pid);
      add_to_run_queue sch proc)
    else (
      Proc_set.add sch.sleep_set proc;
      Log.debug (fun f -> f "Hibernated process %a" Pid.pp proc.pid);
      Log.trace (fun f -> f "sleep_set: %d" (Proc_set.size sch.sleep_set)))

  let handle_exit_proc pool (_sch : t) proc reason =
    Io.unregister_process pool.io_scheduler.io_tbl proc;

    Proc_registry.remove pool.registry (Process.pid proc);

    (* send monitors a process-down message *)
    let monitoring_pids = Process.monitors proc in
    Log.debug (fun f -> f "notifying %d monitors" (List.length monitoring_pids));
    List.iter
      (fun mon_pid ->
        match Proc_table.get pool.processes mon_pid with
        | None -> ()
        | Some mon_proc when Process.is_exited mon_proc ->
            Log.debug (fun f ->
                f "monitoring process %a is dead, nothing to do" Pid.pp
                  mon_proc.pid)
        | Some mon_proc ->
            Log.debug (fun f ->
                f "notified %a of %a terminating" Pid.pp mon_pid Pid.pp proc.pid);
            let msg = Process.Messages.(Monitor (Process_down proc.pid)) in
            Process.send_message mon_proc msg;
            awake_process pool mon_proc)
      monitoring_pids;

    (* mark linked processes as dead *)
    let linked_pids = Process.links proc in
    Log.debug (fun f ->
        f "terminating %d processes linked to %a" (List.length linked_pids)
          Pid.pp proc.pid);
    List.iter
      (fun link_pid ->
        match Proc_table.get pool.processes link_pid with
        | None -> ()
        | Some linked_proc when Atomic.get linked_proc.flags.trap_exits ->
            Log.debug (fun f -> f "%a will trap exits" Pid.pp linked_proc.pid);
            let msg = Process.Messages.(Exit (proc.pid, reason)) in
            Process.send_message linked_proc msg;
            awake_process pool linked_proc
        | Some linked_proc when Process.is_exited linked_proc ->
            Log.debug (fun f ->
                f "linked process %a is already dead, nothing to do" Pid.pp
                  linked_proc.pid)
        | Some linked_proc ->
            let reason = Process.(Link_down proc.pid) in
            Process.mark_as_exited linked_proc reason;
            Log.debug (fun f ->
                f "marking linked %a as dead" Pid.pp linked_proc.pid);
            awake_process pool linked_proc)
      linked_pids

  let handle_run_proc pool (sch : t) proc =
    Log.trace (fun f -> f "Running process %a" Process.pp proc);
    let exception Terminated_while_running of Process.exit_reason in
    try
      Process.mark_as_running proc;
      let perform = perform pool sch proc in
      let cont = Proc_state.run ~reductions:100 ~perform (Process.cont proc) in
      Process.set_cont proc cont;
      match cont with
      | Proc_state.Finished reason ->
          let reason =
            match reason with Ok reason -> reason | Error exn -> Exception exn
          in
          raise_notrace (Terminated_while_running reason)
      | _ when Process.is_waiting_io proc ->
          Log.trace (fun f ->
              f "Process %a hibernated (will resume): %a" Pid.pp proc.pid
                Process.pp proc)
      | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
          Log.trace (fun f ->
              f "Process %a suspended (will resume): %a" Pid.pp proc.pid
                Process.pp proc);
          add_to_run_queue sch proc
    with
    | Process.Process_reviving_is_forbidden _ -> add_to_run_queue sch proc
    | Terminated_while_running reason ->
        Process.mark_as_exited proc reason;
        Log.trace (fun f -> f "Process %a finished" Pid.pp proc.pid);
        add_to_run_queue sch proc

  let step_process pool (sch : t) (proc : Process.t) =
    !Tracer.tracer_proc_run (sch.uid |> Scheduler_uid.to_int) proc;
    match Process.state proc with
    | Finalized -> failwith "finalized processes should never be stepped on"
    | Waiting_io _ -> ()
    | Waiting_message -> handle_wait_proc pool sch proc
    | Exited reason -> handle_exit_proc pool sch proc reason
    | Running | Runnable -> handle_run_proc pool sch proc

  let tick_timers _pool (sch : t) = Timer_wheel.tick sch.timers

  let run pool (sch : t) () =
    Log.trace (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         if pool.stop then raise_notrace Exit;

         Mutex.lock sch.idle_mutex;
         while
           (not pool.stop)
           && Proc_queue.is_empty sch.run_queue
           && not (Timer_wheel.can_tick sch.timers)
         do
           Condition.wait sch.idle_condition sch.idle_mutex
         done;
         Mutex.unlock sch.idle_mutex;

         for _ = 0 to Int.min (Proc_queue.size sch.run_queue) 5_000 do
           match Proc_queue.next sch.run_queue with
           | Some proc ->
               set_current_process_pid proc.pid;
               step_process pool sch proc
           | None -> ()
         done;

         tick_timers pool sch
       done
     with Exit -> ());
    Log.trace (fun f -> f "< exit worker loop")
end

include Scheduler

module Io_scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Log.debug (fun f -> f "Making Io_thread with id: %a" Uid.pp uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      io_tbl = Io.create ();
      idle_mutex = Mutex.create ();
      idle_condition = Condition.create ();
    }

  let poll_io pool io =
    Io.poll io.io_tbl @@ fun (proc, mode) ->
    Log.trace (fun f -> f "io_poll(%a): %a" Fd.Mode.pp mode Process.pp proc);
    match Process.state proc with
    | Waiting_io _ ->
        Process.mark_as_runnable proc;
        awake_process pool proc
    | _ -> ()

  let run pool io () =
    Log.trace (fun f -> f "> enter io loop");
    let exception Exit in
    (try
       while true do
         if pool.stop then raise_notrace Exit;
         poll_io pool io
       done
     with Exit -> ());
    Log.trace (fun f -> f "< exit worker loop")
end

module Pool = struct
  let get_pool, set_pool = Thread_local.make ~name:"POOL"

  let shutdown pool =
    let rec wake_up_scheduler (sch : t) =
      if Mutex.try_lock sch.idle_mutex then (
        Condition.signal sch.idle_condition;
        Mutex.unlock sch.idle_mutex)
      else wake_up_scheduler sch
    in
    Log.trace (fun f -> f "shutdown called");
    pool.stop <- true;
    List.iter wake_up_scheduler pool.schedulers

  let register_process pool _scheduler proc =
    Proc_table.register_process pool.processes proc

  let setup () =
    (* NOTE(@leostera): we want the Net subsystem to be able to write to closed
       sockets and handle that as a regular value rather than as a signal. *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore

  let make ?(rnd = Random.State.make_self_init ()) ~domains ~main () =
    setup ();

    Log.debug (fun f -> f "Making scheduler pool...");
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in

    let io_scheduler = Io_scheduler.make ~rnd () in
    let pool =
      {
        stop = false;
        io_scheduler;
        schedulers = [ main ] @ schedulers;
        processes = Proc_table.create ();
        registry = Proc_registry.create ();
      }
    in
    let spawn (scheduler : t) =
      Stdlib.Domain.spawn (fun () ->
          set_pool pool;
          Scheduler.set_current_scheduler scheduler;
          try
            Scheduler.run pool scheduler ();
            Log.trace (fun f ->
                f "<<< shutting down scheduler #%a" Scheduler_uid.pp
                  scheduler.uid)
          with exn ->
            Log.error (fun f ->
                f "Scheduler.run exception: %s due to: %s%!"
                  (Printexc.to_string exn)
                  (Printexc.raw_backtrace_to_string
                     (Printexc.get_raw_backtrace ())));
            shutdown pool)
    in
    Log.debug (fun f -> f "Created %d schedulers" (List.length schedulers));

    let io_thread =
      Stdlib.Domain.spawn (fun () ->
          try Io_scheduler.run pool io_scheduler ()
          with exn ->
            Log.error (fun f ->
                f "Io_scheduler.run exception: %s due to: %s%!"
                  (Printexc.to_string exn)
                  (Printexc.raw_backtrace_to_string
                     (Printexc.get_raw_backtrace ())));
            shutdown pool)
    in

    let scheduler_threads = List.map spawn schedulers in
    (pool, io_thread :: scheduler_threads)
end
