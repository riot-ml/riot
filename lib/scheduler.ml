module Uid = Scheduler_uid

type t = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  run_queue : Proc_queue.t;
  sleep_set : Proc_set.t;
  io: Io.t;
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
      sleep_set = Proc_set.create ();
      io_fds = Hashtbl.create 1024;
    }

  let get_current_scheduler, set_current_scheduler =
    Thread_local.make ~name:"CURRENT_SCHEDULER"

  let get_current_process_pid, set_current_process_pid =
    Thread_local.make ~name:"CURRENT_PID"

  let get_random_scheduler : pool -> t =
   fun { schedulers = all_schedulers; _ } ->
    let sch = get_current_scheduler () in
    let rnd_idx = Random.State.int sch.rnd (List.length all_schedulers) in
    List.nth all_schedulers rnd_idx

  let add_to_run_queue sch (proc : Process.t) =
    Proc_set.remove sch.sleep_set proc;
    Proc_queue.queue sch.run_queue proc;
    Logs.trace (fun f ->
        f "Adding process to run_queue queue: %a" Pid.pp proc.pid)

  let awake_process pool (proc : Process.t) =
    List.iter
      (fun sch ->
        if Scheduler_uid.equal sch.uid proc.sid then add_to_run_queue sch proc)
      pool.schedulers

  let perform _sch (proc : Process.t) =
    let open Proc_state in
    let open Proc_effect in
    let perform : type a b. (a, b) step_callback =
     fun k eff ->
      Logs.trace (fun f ->
          f "Process %a: performing effect: %a" Pid.pp (Process.pid proc)
            Proc_effect.pp eff);
      match eff with
      | Yield ->
          Logs.trace (fun f ->
              f "Process %a: yielding" Pid.pp (Process.pid proc));
          k Yield
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
          Logs.trace (fun f ->
              f "Process %a: receiving messages" Pid.pp (Process.pid proc));
          if Process.has_empty_mailbox proc then (
            Logs.debug (fun f ->
                f "Process %a is awaiting for new messages" Pid.pp
                  (Process.pid proc));
            Process.mark_as_awaiting_message proc;
            k Delay)
          else
            let skipped = Mailbox.create () in
            let rec go () =
              (* NOTE(leostera): we can get the value out of the option because
                 the case above checks for an empty mailbox. *)
              match Process.next_message proc with
              | None ->
                  let rec move () =
                    match Mailbox.next skipped with
                    | Some msg ->
                        Process.send_message proc msg;
                        move ()
                    | None -> ()
                  in
                  move ();
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
      | effect ->
          Logs.trace (fun f ->
              f "Process %a: unhandled effect" Pid.pp (Process.pid proc));
          k (Reperform effect)
    in
    { perform }

  let step_process pool sch (proc : Process.t) =
    Logs.trace (fun f -> f "Stepping process %a" Process.pp proc);
    !Tracer.tracer_proc_run (sch.uid |> Scheduler_uid.to_int) proc;
    match Process.state proc with
    | Waiting when Process.has_messages proc ->
        Process.mark_as_runnable proc;
        Logs.debug (fun f -> f "waking up process %a" Pid.pp proc.pid);
        add_to_run_queue sch proc
    | Waiting ->
        Proc_set.add sch.sleep_set proc;
        Logs.debug (fun f -> f "hibernated process %a" Pid.pp proc.pid);
        Logs.trace (fun f -> f "sleep_set: %d" (Proc_set.size sch.sleep_set))
    | Exited reason ->
        (* send monitors a process-down message *)
        let monitoring_pids = Process.monitors proc in
        Logs.debug (fun f ->
            f "notifying %d monitors" (List.length monitoring_pids));
        List.iter
          (fun mon_pid ->
            match Proc_table.get pool.processes mon_pid with
            | None -> ()
            | Some mon_proc when Process.is_exited mon_proc ->
                Logs.debug (fun f ->
                    f "monitoring process %a is dead, nothing to do" Pid.pp
                      mon_proc.pid)
            | Some mon_proc ->
                Logs.debug (fun f ->
                    f "notified %a of %a terminating" Pid.pp mon_pid Pid.pp
                      proc.pid);
                let msg = Process.Messages.(Monitor (Process_down proc.pid)) in
                Process.send_message mon_proc msg;
                awake_process pool mon_proc)
          monitoring_pids;

        (* mark linked processes as dead *)
        let linked_pids = Process.links proc in
        Logs.debug (fun f ->
            f "terminating %d processes linked to %a" (List.length linked_pids)
              Pid.pp proc.pid);
        List.iter
          (fun link_pid ->
            match Proc_table.get pool.processes link_pid with
            | None -> ()
            | Some linked_proc when Atomic.get linked_proc.flags.trap_exits ->
                Logs.debug (fun f ->
                    f "%a will trap exits" Pid.pp linked_proc.pid);
                let msg = Process.Messages.(Exit (proc.pid, reason)) in
                Process.send_message linked_proc msg;
                awake_process pool linked_proc
            | Some linked_proc when Process.is_exited linked_proc ->
                Logs.debug (fun f ->
                    f "linked process %a is already dead, nothing to do" Pid.pp
                      linked_proc.pid)
            | Some linked_proc ->
                Logs.debug (fun f ->
                    f "marking linked %a as dead" Pid.pp linked_proc.pid);
                let reason = Process.(Link_down linked_proc.pid) in
                Process.mark_as_exited linked_proc reason;
                awake_process pool linked_proc)
          linked_pids
    | Running | Runnable -> (
        Logs.trace (fun f -> f "Running process %a" Process.pp proc);
        let exception Terminated_while_running of Process.exit_reason in
        try
          Process.mark_as_running proc;
          let perform = perform sch proc in
          let cont = Proc_state.run ~perform (Process.cont proc) in
          Process.set_cont proc cont;
          match cont with
          | Proc_state.Finished reason ->
              let reason =
                match reason with
                | Ok reason -> reason
                | Error exn -> Exception exn
              in
              raise_notrace (Terminated_while_running reason)
          | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
              Logs.trace (fun f ->
                  f "Process %a suspended (will resume): %a" Pid.pp proc.pid
                    Process.pp proc);
              add_to_run_queue sch proc
        with
        | Process.Process_reviving_is_forbidden _ -> add_to_run_queue sch proc
        | Terminated_while_running reason ->
            Process.mark_as_exited proc reason;
            Logs.trace (fun f -> f "Process %a finished" Pid.pp proc.pid);
            add_to_run_queue sch proc)

  let run pool sch () =
    Logs.trace (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         if pool.stop then raise_notrace Exit;

         match Proc_queue.next sch.run_queue with
         | Some proc ->
             set_current_process_pid proc.pid;
             step_process pool sch proc
         | None -> ()
       done
     with Exit -> ());
    Logs.trace (fun f -> f "< exit worker loop")
end

include Scheduler

module Pool = struct
  let get_pool, set_pool = Thread_local.make ~name:"POOL"

  let shutdown pool =
    Logs.trace (fun f -> f "shutdown called");
    pool.stop <- true

  let register_process pool _scheduler proc =
    Proc_table.register_process pool.processes proc

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
