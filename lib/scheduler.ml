module Uid = Scheduler_uid

type t = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  mutable sleep : bool;
  run_queue : Proc_queue.t;
  proc_set : Proc_set.t;
  sleep_set : Proc_set.t;
  idle_lock : Mutex.t;
  idle_cond : Condition.t;
}

type pool = {
  mutable stop : bool;
  schedulers : t list;
  processes : Proc_table.t;
}

module Scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Logs.debug (fun f -> f "Making scheduler with id: %a" Uid.pp uid);
    {
      uid;
      rnd = Random.State.copy rnd;
      run_queue = Proc_queue.create ();
      proc_set = Proc_set.create ();
      sleep_set = Proc_set.create ();
      idle_lock = Mutex.create ();
      idle_cond = Condition.create ();
      sleep = false;
    }

  let get_current_scheduler, set_current_scheduler =
    Thread_local.make ~name:"CURRENT_SCHEDULER"

  let get_current_process_pid, set_current_process_pid =
    Thread_local.make ~name:"CURRENT_PID"

  let get_random_scheduler : pool -> t =
   fun { schedulers = all_schedulers; _ } ->
    let scheduler = get_current_scheduler () in
    let rnd_idx = Random.State.int scheduler.rnd (List.length all_schedulers) in
    List.nth all_schedulers rnd_idx

  let add_to_run_queue sch pid =
    Mutex.lock sch.idle_lock;
    Logs.trace (fun f -> f "Adding process to run_queue queue: %a" Pid.pp pid);
    Proc_queue.queue sch.run_queue pid;
    Condition.signal sch.idle_cond;
    Mutex.unlock sch.idle_lock

  let awake_process sch (proc : Process.t) =
    Logs.trace (fun f -> f "Awaking process %a" Process.pp proc);
    Proc_set.remove sch.sleep_set proc.pid;
    add_to_run_queue sch proc.pid

  let awake_process pool (proc : Process.t) =
    List.iter
      (fun sch ->
        if Scheduler_uid.equal sch.uid proc.sid then awake_process sch proc)
      pool.schedulers

  let perform _scheduler (process : Process.t) =
    let open Proc_state in
    let open Proc_effect in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.trace (fun f -> f "performing effect: %a" Proc_effect.pp eff);
      match eff with
      | Yield -> k Yield
      (* NOTE(leostera): the selective receive algorithm goes:

         * is there a new message?
           -> no: reperform – we will essentially be blocked here until we
                  either receive a message or we timeout (if a timeout is set)
           -> yes: check if we should take the message
              -> take: return the message and continue
              -> skip: put the message on a temporary skip queue
         * loop until the mailbox is
      *)
      | Receive { select } ->
          if Process.has_empty_mailbox process then (
            Logs.debug (fun f ->
                f "%a is awaiting for new messages" Pid.pp process.pid);
            Process.mark_as_awaiting_message process;
            k Delay)
          else
            let skipped = Mailbox.create () in
            let rec go () =
              (* NOTE(leostera): we can get the value out of the option because
                 the case above checks for an empty mailbox. *)
              match Mailbox.next process.mailbox with
              | None ->
                  Mailbox.merge process.mailbox skipped;
                  k Delay
              | Some msg -> (
                  match select msg with
                  | Drop -> go ()
                  | Take -> k (Continue msg)
                  | Skip ->
                      Mailbox.queue skipped msg;
                      go ())
            in
            go ()
      | effect -> k (Reperform effect)
    in
    { perform }

  let step_process pool scheduler (proc : Process.t) =
    set_current_process_pid proc.pid;
    match Process.state proc with
    | Waiting ->
        Proc_set.add scheduler.sleep_set proc.pid;
        Logs.debug (fun f -> f "hibernated process %a" Pid.pp proc.pid);
        Logs.trace (fun f ->
            f "sleep_set: %d" (Proc_set.size scheduler.sleep_set))
    | Exited reason ->
        (* send monitors a process-down message *)
        let monitoring_pids = Atomic.get proc.monitors in
        List.iter
          (fun pid ->
            match Proc_table.get pool.processes pid with
            | None -> ()
            | Some mon_proc ->
                Logs.debug (fun f ->
                    f "notified %a of %a terminating" Pid.pp pid Pid.pp proc.pid);
                let msg = Process.Messages.(Monitor (Process_down proc.pid)) in
                Process.send_message mon_proc msg;
                awake_process pool mon_proc)
          monitoring_pids;

        (* mark linked processes as dead *)
        let linked_pids = Atomic.get proc.links in
        Logs.debug (fun f ->
            f "terminating %d processes linked to %a" (List.length linked_pids)
              Pid.pp proc.pid);
        List.iter
          (fun pid ->
            match Proc_table.get pool.processes pid with
            | None -> ()
            | Some linked_proc when linked_proc.flags.trap_exits ->
                Logs.debug (fun f ->
                    f "%a will trap exits" Pid.pp linked_proc.pid);
                let msg = Process.Messages.(Exit (proc.pid, reason)) in
                Process.send_message proc msg;
                awake_process pool linked_proc
            | Some linked_proc ->
                Logs.debug (fun f ->
                    f "marking linked %a as dead" Pid.pp linked_proc.pid);
                Process.mark_as_dead linked_proc Exit_signal;
                awake_process pool linked_proc)
          linked_pids
    | Running | Runnable -> (
        Process.mark_as_running proc;
        let perform = perform scheduler proc in
        let cont = Proc_state.run ~perform (Process.cont proc) in
        Process.set_cont cont proc;
        match cont with
        | Proc_state.Finished reason ->
            let reason =
              match reason with
              | Ok reason -> reason
              | Error exn -> Exception exn
            in
            Process.mark_as_dead proc reason;
            add_to_run_queue scheduler proc.pid
        | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
            add_to_run_queue scheduler proc.pid)

  let should_wait pool scheduler =
    Proc_queue.is_empty scheduler.run_queue && not pool.stop

  let run pool scheduler () =
    Logs.trace (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         Mutex.lock scheduler.idle_lock;

         while should_wait pool scheduler do
           Logs.trace (fun f -> f "idle scheduler");
           scheduler.sleep <- true;
           Condition.wait scheduler.idle_cond scheduler.idle_lock
         done;
         scheduler.sleep <- false;

         if pool.stop then raise_notrace Exit;
         Mutex.unlock scheduler.idle_lock;

         (match Proc_queue.next scheduler.run_queue with
         | None -> Logs.trace (fun f -> f "no ready processes")
         | Some pid ->
             let proc = Proc_table.get pool.processes pid |> Option.get in
             Logs.trace (fun f -> f "found process: %a" Process.pp proc);
             step_process pool scheduler proc);

         ()
       done
     with Exit -> ());
    Logs.trace (fun f -> f "< exit worker loop")
end

include Scheduler

module Pool = struct
  let get_pool, set_pool = Thread_local.make ~name:"POOL"

  let shutdown pool =
    Logs.trace (fun f -> f "shutdown called");
    pool.stop <- true;
    List.iter (fun sch -> Condition.broadcast sch.idle_cond) pool.schedulers

  let register_process pool scheduler proc =
    Proc_table.register_process pool.processes proc;
    Proc_set.add scheduler.proc_set proc.pid

  let make ?(rnd = Random.State.make_self_init ()) ~domains ~main () =
    Logs.debug (fun f -> f "Making scheduler pool...");
    let schedulers = List.init domains @@ fun _ -> Scheduler.make ~rnd () in
    let pool =
      {
        stop = false;
        schedulers = [ main ] @ schedulers;
        processes = Proc_table.create ();
      }
    in
    let spawn scheduler =
      Stdlib.Domain.spawn (fun () ->
          set_pool pool;
          Scheduler.set_current_scheduler scheduler;
          try
            Scheduler.run pool scheduler ();
            Logs.trace (fun f ->
                f "<<< shutting down scheduler #%a" Scheduler_uid.pp
                  scheduler.uid)
          with exn ->
            shutdown pool;
            Logs.error (fun f ->
                f "Scheduler.run exception: %s due to: %s%!"
                  (Printexc.to_string exn)
                  (Printexc.raw_backtrace_to_string
                     (Printexc.get_raw_backtrace ()))))
    in
    Logs.debug (fun f -> f "Created %d schedulers" (List.length schedulers));
    (pool, List.map spawn schedulers)
end
