open Core
open Util
open Time
module Tracer = Tracer
module Uid = Scheduler_uid

type t = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  run_queue : Proc_queue.t;
  timers : Timer_wheel.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
  currently_stealing : Mutex.t;
  mutable stop : bool;
}

type io = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  poll : Gluon.Poll.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
  mutable calls_accept : int;
  mutable calls_connect : int;
  mutable calls_receive : int;
  mutable calls_send : int;
}

type blocking = { scheduler : t; domain : unit Domain.t }

type pool = {
  mutable stop : bool;
  mutable status : int;
  io_scheduler : io;
  schedulers : t list;
  processes : Proc_table.t;
  blocking_schedulers : blocking list Atomic.t;
  mutable proc_count : int;
  registry : Proc_registry.t;
}

let shutdown pool status =
  let rec wake_up_scheduler (sch : t) =
    if Mutex.try_lock sch.idle_mutex then (
      Condition.signal sch.idle_condition;
      Mutex.unlock sch.idle_mutex)
    else wake_up_scheduler sch
  in
  Log.trace (fun f -> f "shutdown called");
  pool.status <- status;
  pool.stop <- true;
  List.iter wake_up_scheduler pool.schedulers

module Scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Log.debug (fun f -> f "Making scheduler with id: %a" Uid.pp uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      run_queue = Proc_queue.create ();
      timers = Timer_wheel.create ();
      idle_mutex = Mutex.create ();
      idle_condition = Condition.create ();
      currently_stealing = Mutex.create ();
      stop = false;
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

  let remove_timer sch timer = Timer_wheel.remove_timer sch.timers timer

  let add_to_run_queue (sch : t) (proc : Process.t) =
    Mutex.protect sch.idle_mutex @@ fun () ->
    Proc_queue.queue sch.run_queue proc;
    Condition.signal sch.idle_condition;
    Log.trace (fun f ->
        f "Adding process to run_queue queue[%d]: %a"
          (Proc_queue.size sch.run_queue)
          Pid.pp proc.pid)

  let awake_process pool (proc : Process.t) =
    List.iter
      (fun (sch : t) ->
        if Scheduler_uid.equal sch.uid (Atomic.get proc.sid) then
          add_to_run_queue sch proc)
      pool.schedulers

  let kickstart_blocking_process pool sch (proc : Process.t) =
    add_to_run_queue sch proc;
    pool.schedulers

  let handle_receive k pool sch (proc : Process.t) ~(ref : 'a Ref.t option)
      ~timeout ~selector =
    Trace.handle_receive_span @@ fun () ->
    let open Proc_state in
    (* When a timeout is specified, we want to create it in the timer
       wheel, and save the timer reference in the process.

       That way, whenever we gets re-scheduled to run this specific `Receive` effect
       we can check on the Process to see if it has timed out.
    *)
    let should_timeout, _next_step =
      match (Process.receive_timeout proc, timeout) with
      | Some timeout, _ ->
          let finished = Timer_wheel.is_finished sch.timers timeout in
          if finished then Timer_wheel.clear_timer sch.timers timeout;
          Log.debug (fun f ->
              f "Process %a: process receive timeout? %b" Pid.pp
                (Process.pid proc) finished);
          (finished, `receiving)
      | None, `infinity -> (false, `receiving)
      | None, `after s ->
          Log.trace (fun f ->
              f "Process %a: creating receive timeout" Pid.pp (Process.pid proc));
          let timeout =
            Timer_wheel.make_timer sch.timers s `one_off (fun () ->
                Log.trace (fun f ->
                    f "Process %a: TIMEOUT" Pid.pp (Process.pid proc));
                if Process.is_alive proc then (
                  Process.mark_as_runnable proc;
                  awake_process pool proc))
          in
          Process.set_receive_timeout proc timeout;
          (false, `set_timeout)
    in

    Log.trace (fun f ->
        f "Process %a: receiving messages (timeout? %b | empty_mailbox? %b)"
          Pid.pp (Process.pid proc) should_timeout
          (Process.has_empty_mailbox proc));

    if should_timeout then (
      Log.trace (fun f -> f "Process %a: timed out" Pid.pp (Process.pid proc));
      Process.clear_receive_timeout proc;
      k (Discontinue Process.Exn.Receive_timeout))
    else if Process.has_empty_mailbox proc then (
      Log.trace (fun f ->
          f "Process %a is awaiting for new messages" Pid.pp (Process.pid proc));
      Process.mark_as_awaiting_message proc;
      k Suspend)
    else
      (* NOTE(@leostera): we will use the current message count as fuel to stop the
         recursion. This count may increase as we are iterating, but that's okay.
         If we consume it entirely, the receive will just be delayed until the next
         scheduled run, and at that point we'll pick up any new messages.
      *)
      let fuel = Process.message_count proc in

      Log.trace (fun f -> f "Skimming mailbox with %d messages" fuel);
      let rec go fuel =
        if fuel = 0 then k Delay
        else
          match (ref, Process.next_message proc) with
          (* if the mailbox is empty, we will switch to reading messages from
             the save queue, and delay execution to the next iteration *)
          | _, None ->
              Log.trace (fun f ->
                  f "Emptied the queue, will read from save queue next");
              Process.read_save_queue proc;
              k Delay
          (* when we have an explicit ref, we will skip any message that was created
             BEFORE this ref was created, which enables the selective receive.

             Any skipped messages will go in the same order as received into
             the save queue, which will be read after the mailbox is depleted.
          *)
          | Some ref, Some msg when Ref.is_newer ref msg.uid ->
              Log.trace (fun f ->
                  f "Skipping msg ref=%a msg.uid=%a" Ref.pp ref Ref.pp msg.uid);
              Process.add_to_save_queue proc msg;
              go (fuel - 1)
          (* we are special casing the process monitors here. if we receive a process down
             but we already have removed the monitor, then we will simply ignore this message. *)
          | ( _,
              Some
                (Message.
                   {
                     msg =
                       Process.Messages.Monitor (Process_down mon_pid) as msg;
                     _;
                   } as envelope) ) ->
              Process.clear_receive_timeout proc;
              if Process.is_monitored_by_pid proc mon_pid then (
                match selector msg with
                | `select msg ->
                    Process.clear_receive_timeout proc;
                    k (Continue msg)
                | `skip ->
                    Process.add_to_save_queue proc envelope;
                    go (fuel - 1))
              else go (fuel - 1)
          (* lastly, if we have a ref and the mesasge is newer than the ref, and
             when we don't have a ref, we just pop the message and continue with it
          *)
          | _, Some msg -> (
              match selector Message.(msg.msg) with
              | `select msg ->
                  Process.clear_receive_timeout proc;
                  k (Continue msg)
              | `skip ->
                  Process.add_to_save_queue proc msg;
                  go (fuel - 1))
      in

      go fuel

  let handle_syscall k pool (sch : t) (proc : Process.t) name interest source
      timeout =
    Trace.handle_syscall_span @@ fun () ->
    let open Proc_state in
    let should_timeout =
      match Process.syscall_timeout proc with
      | Some timeout -> Timer_wheel.is_finished sch.timers timeout
      | None -> false
    in

    Log.debug (fun f ->
        f "handle_syscall %s with %a (timeout? %b)" name Process.pp proc
          should_timeout);

    if Process.is_exited proc then k (Discontinue Process.Exn.Terminated)
    else if should_timeout then (
      Log.debug (fun f -> f "Process %a: timed out" Pid.pp (Process.pid proc));
      Process.syscall_timeout proc
      |> Option.iter (Timer_wheel.clear_timer sch.timers);
      Process.clear_syscall_timeout proc;
      k (Discontinue Process.Exn.Syscall_timeout))
    else
      match Process.get_ready_token proc with
      | Some (token, _source) ->
          Log.debug (fun f ->
              f "syscall(%s) %a -> %a is ready" name Pid.pp proc.pid
                Gluon.Token.pp token);
          (match Process.syscall_timeout proc with
          | Some timeout ->
              Timer_wheel.clear_timer sch.timers timeout;
              Process.clear_syscall_timeout proc
          | None -> ());
          k (Continue ())
      | None ->
          let token = Gluon.Token.make proc in
          Log.debug (fun f ->
              f "syscall(%s) %a -> registering %a" name Pid.pp proc.pid
                Gluon.Token.pp token);
          Process.mark_as_awaiting_io proc name token source;
          Gluon.Poll.register pool.io_scheduler.poll token interest source
          |> Result.get_ok;

          (* once we've set up the poll, we can set up a timeout *)
          (match timeout with
          | `infinity -> ()
          | `after s ->
              Log.debug (fun f ->
                  f "Process %a: creating syscall timeout" Pid.pp
                    (Process.pid proc));
              let timeout =
                Timer_wheel.make_timer sch.timers s `one_off (fun () ->
                    Log.debug (fun f ->
                        f "Process %a: TIMEOUT" Pid.pp (Process.pid proc));
                    if Process.is_alive proc then (
                      Process.mark_as_runnable proc;
                      awake_process pool proc))
              in
              Process.set_syscall_timeout proc timeout);

          k Suspend

  let perform pool (sch : t) (proc : Process.t) =
    let open Proc_state in
    let open Proc_effect in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      match eff with
      | Receive { ref; timeout; selector } ->
          handle_receive k pool sch proc ~ref ~timeout ~selector
      | Syscall { name; interest; source; timeout } ->
          handle_syscall k pool sch proc name interest source timeout
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
    Trace.handle_wait_proc_span @@ fun () ->
    if Process.has_messages proc then (
      Process.mark_as_runnable proc;
      Log.debug (fun f -> f "Waking up process %a" Pid.pp proc.pid);
      add_to_run_queue sch proc)
    else Log.debug (fun f -> f "Hibernated process %a" Pid.pp proc.pid)

  let handle_exit_proc pool (sch : t) proc (reason : Process.exit_reason) =
    Trace.handle_exit_proc_span @@ fun () ->
    Log.debug (fun f -> f "exiting process %a" Pid.pp (Process.pid proc));

    Process.consume_ready_tokens proc (fun (_token, source) ->
        Gluon.Poll.deregister pool.io_scheduler.poll source |> Result.get_ok);

    Proc_registry.remove pool.registry (Process.pid proc);

    (* if it's main process we want to terminate the entire program *)
    (if Process.is_main proc then
       let status = if reason = Process.Normal then 0 else 1 in
       shutdown pool status);

    (* send monitors a process-down message *)
    let monitoring_pids = Process.monitors proc |> List.of_seq in
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
      linked_pids;

    if Process.is_blocking_proc proc then (
      Log.debug (fun f -> f "Set scheduler.stop to true");
      sch.stop <- true)
    else ();

    Proc_queue.remove sch.run_queue proc;
    Proc_table.remove pool.processes proc.pid;
    Proc_registry.remove pool.registry proc.pid;
    Process.free proc;
    Log.debug (fun f -> f "terminated %a" Pid.pp proc.pid)

  let handle_run_proc pool (sch : t) proc =
    Log.trace (fun f -> f "Running process %a" Process.pp proc);
    let exception Terminated_while_running of Process.exit_reason in
    try
      Process.mark_as_running proc;
      let perform = perform pool sch proc in
      Log.debug (fun f -> f "Running process %a" Process.pp proc);
      let cont =
        Proc_state.run ~reductions:1000 ~perform (Process.cont proc)
        |> Option.get
      in
      Log.trace (fun f ->
          f "Process %a state: %a" Pid.pp proc.pid Proc_state.pp cont);
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
    with Terminated_while_running reason ->
      Process.mark_as_exited proc reason;
      Log.trace (fun f -> f "Process %a finished" Pid.pp proc.pid);
      add_to_run_queue sch proc

  let handle_init_proc pool sch proc =
    Process.init proc;
    handle_run_proc pool sch proc

  let step_process pool (sch : t) (proc : Process.t) =
    match Process.state proc with
    | Uninitialized -> handle_init_proc pool sch proc
    | Finalized -> failwith "finalized processes should never be stepped on"
    | Waiting_io _ -> ()
    | Waiting_message -> handle_wait_proc pool sch proc
    | Exited reason -> handle_exit_proc pool sch proc reason
    | Running | Runnable ->
        Trace.handle_run_proc_start ();
        handle_run_proc pool sch proc;
        Trace.handle_run_proc_finish ()

  let tick_timers _pool (sch : t) = Timer_wheel.tick sch.timers

  let steal_processes pool (us : t) =
    List.iter
      (fun (them : t) ->
        (* NOTE(@leostera): avoid stealing from ourselves :) *)
        if Scheduler_uid.equal us.uid them.uid then ()
        else
          match Proc_queue.next them.run_queue with
          | None -> ()
          | Some proc ->
              Log.debug (fun f ->
                  f "scheduler %a stole %a from %a" Scheduler_uid.pp them.uid
                    Pid.pp proc.pid Scheduler_uid.pp us.uid);
              Process.set_sid proc them.uid;
              Proc_queue.queue them.run_queue proc)
      pool.schedulers

  let run_loop pool (sch : t) =
    Trace.scheduler_loop_begin ();

    for _ = 0 to Int.min (Proc_queue.size sch.run_queue) 500 do
      match Proc_queue.next sch.run_queue with
      | Some proc ->
          set_current_process_pid proc.pid;
          step_process pool sch proc
      | None -> ()
    done;

    tick_timers pool sch;
    (* if Proc_queue.is_empty sch.run_queue then steal_processes pool sch; *)
    Trace.scheduler_loop_end ()

  let run pool (sch : t) () =
    Log.trace (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         if pool.stop then raise_notrace Exit;
         if sch.stop then raise_notrace Exit;

         Mutex.lock sch.idle_mutex;
         while
           (not pool.stop)
           && Proc_queue.is_empty sch.run_queue
           && not (Timer_wheel.can_tick sch.timers)
         do
           Condition.wait sch.idle_condition sch.idle_mutex
         done;
         Mutex.unlock sch.idle_mutex;

         run_loop pool sch
       done
     with Exit -> ());
    Log.trace (fun f -> f "< exit worker loop")
end

module Blocking_scheduler = struct
  (* include Scheduler *)
  type t = blocking

  let make sch domain = { scheduler = sch; domain }

  let rec add_to_pool pool blocking =
    let dom_list = Atomic.get pool.blocking_schedulers in
    if
      Atomic.compare_and_set pool.blocking_schedulers dom_list
        (blocking :: dom_list)
    then ()
    else add_to_pool pool blocking

  let rec remove_from_pool pool blocking =
    let cur = Atomic.get pool.blocking_schedulers in
    let without_removee = List.filter (fun sch -> sch.domain != blocking.domain) cur in
    if Atomic.compare_and_set pool.blocking_schedulers cur without_removee then
      ()
    else remove_from_pool pool blocking


  (* Override the handle exit function *)
  (* let handle_exit_blocking_proc pool sch proc reason = *)
    (* Scheduler.handle_exit_proc pool sch.scheduler proc reason; *)
    (* remove_from_pool pool sch *)
end

include Scheduler

module Io_scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Log.debug (fun f -> f "Making Io_thread with id: %a" Uid.pp uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      poll = Gluon.Poll.make () |> Result.get_ok;
      idle_mutex = Mutex.create ();
      idle_condition = Condition.create ();
      calls_accept = 0;
      calls_send = 0;
      calls_connect = 0;
      calls_receive = 0;
    }

  let poll_io pool io =
    Trace.poll_io_span @@ fun () ->
    let events = Gluon.Poll.poll io.poll |> Result.get_ok in
    List.iter
      (fun event ->
        let token = Gluon.Event.token event in
        let proc : Process.t = Gluon.Token.unsafe_to_value token in
        Log.debug (fun f ->
            f "polled %a - %a" Gluon.Token.pp token Pid.pp proc.pid);
        match Process.state proc with
        | Waiting_io { source; _ } ->
            Log.debug (fun f ->
                f "awaking proc %a %a" Process.pp proc Gluon.Token.pp token);
            Gluon.Poll.deregister io.poll source |> Result.get_ok;
            if Process.is_alive proc then (
              Process.add_ready_token proc token source;
              Process.mark_as_runnable proc;
              awake_process pool proc)
        | _ -> ())
      events

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
  let shutdown = shutdown

  let register_process pool proc =
    pool.proc_count <- pool.proc_count + 1;
    Proc_table.register_process pool.processes proc

  let setup () =
    (* NOTE(@leostera): we want the Net subsystem to be able to write to closed
       sockets and handle that as a regular value rather than as a signal. *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore

  let spawn_scheduler_on_pool pool (scheduler : t) : unit Domain.t =
    Stdlib.Domain.spawn (fun () ->
        setup ();
        Stdlib.Domain.at_exit (fun () -> Log.warn (fun f -> f "Domain freed"));
        set_pool pool;
        Scheduler.set_current_scheduler scheduler;
        try
          Scheduler.run pool scheduler ();
          Log.trace (fun f ->
              f "<<< shutting down scheduler #%a" Scheduler_uid.pp scheduler.uid)
        with exn ->
          Log.error (fun f ->
              f "Scheduler.run exception: %s due to: %s%!"
                (Printexc.to_string exn)
                (Printexc.raw_backtrace_to_string
                   (Printexc.get_raw_backtrace ())));
          shutdown pool 1)

  let make ?(rnd = Random.State.make_self_init ()) ~domains ~main () =
    setup ();

    Log.debug (fun f -> f "Making scheduler pool...");
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in

    let io_scheduler = Io_scheduler.make ~rnd () in
    let pool =
      {
        stop = false;
        status = 0;
        proc_count = 0;
        io_scheduler;
        schedulers = [ main ] @ schedulers;
        processes = Proc_table.create ();
        blocking_schedulers = Atomic.make [];
        registry = Proc_registry.create ();
      }
    in
    Log.debug (fun f ->
        f "Created %d schedulers excluding the main scheduler"
          (List.length schedulers));

    let io_thread =
      Stdlib.Domain.spawn (fun () ->
          try Io_scheduler.run pool io_scheduler ()
          with exn ->
            Log.error (fun f ->
                f "Io_scheduler.run exception: %s due to: %s%!"
                  (Printexc.to_string exn)
                  (Printexc.raw_backtrace_to_string
                     (Printexc.get_raw_backtrace ())));
            shutdown pool 2)
    in

    let scheduler_threads =
      List.map (spawn_scheduler_on_pool pool) schedulers
    in
    (pool, io_thread :: scheduler_threads)

  (** Creates a new blocking scheduler in the pool *)
  let spawn_blocking_scheduler ?(rnd = Random.State.make_self_init ()) pool =
    let new_scheduler = Scheduler.make ~rnd () in
    let domain = spawn_scheduler_on_pool pool new_scheduler in
    let blocking_sch = Blocking_scheduler.make new_scheduler domain in
    Blocking_scheduler.add_to_pool pool blocking_sch;
    blocking_sch
end
