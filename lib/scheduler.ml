module Uid = Scheduler_uid

type t = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  ready_queue : Process.t Lf_queue.t;
}

type pool = {
  mutable stop : bool;
  schedulers : t list;
  processes : Proc_table.t;
}

module Scheduler = struct
  let make ~rnd () =
    let uid = Uid.next () in
    Logs.debug (fun f -> f "Making scheduler with id: %d" uid);
    { uid; rnd = Random.State.copy rnd; ready_queue = Lf_queue.create () }

  let get_current_scheduler, set_current_scheduler =
    Thread_local.make ~name:"CURRENT_SCHEDULER"

  let get_current_process_pid, set_current_process_pid =
    Thread_local.make ~name:"CURRENT_PID"

  let get_random_scheduler : pool -> t =
   fun { schedulers = all_schedulers; _ } ->
    let scheduler = get_current_scheduler () in
    let rnd_idx = Random.State.int scheduler.rnd (List.length all_schedulers) in
    List.nth all_schedulers rnd_idx

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
      | Receive { select } as effect ->
          if Mailbox.is_empty process.mailbox then (
            Logs.debug (fun f ->
                f "%a is awaiting for new messages" Pid.pp process.pid);
            Process.mark_as_awaiting_message process;
            k (Delay effect))
          else
            let skipped = Mailbox.create () in
            let rec go () =
              (* NOTE(leostera): we can get the value out of the option because
                 the case above checks for an empty mailbox. *)
              match Mailbox.next process.mailbox with
              | None ->
                  Mailbox.merge process.mailbox skipped;
                  k (Delay effect)
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
    | Waiting -> Lf_queue.add proc scheduler.ready_queue
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
                Mailbox.queue mon_proc.mailbox
                  Process.Messages.(Monitor (Process_down proc.pid));
                Process.mark_as_runnable mon_proc)
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
                Mailbox.queue linked_proc.mailbox
                  Process.Messages.(Exit (proc.pid, reason));
                Process.mark_as_runnable linked_proc
            | Some linked_proc ->
                Logs.debug (fun f ->
                    f "marking linked %a as dead" Pid.pp linked_proc.pid);
                Process.mark_as_dead linked_proc Exit_signal)
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
            Lf_queue.add proc scheduler.ready_queue
        | Proc_state.Suspended _ | Proc_state.Unhandled _ ->
            Lf_queue.add proc scheduler.ready_queue)

  let is_idle t = Lf_queue.is_empty t.ready_queue

  let run pool scheduler () =
    Logs.debug (fun f -> f "> enter worker loop");
    let exception Exit in
    (try
       while true do
         if pool.stop then raise_notrace Exit;
         match Lf_queue.take_opt scheduler.ready_queue with
         | None ->
             (* TODO(leostera): if we have no ready processes, we should wait
                until a process becomes ready *)
             Logs.trace (fun f -> f "no ready processes");
             Domain.cpu_relax ()
         | Some proc ->
             Logs.trace (fun f -> f "found process: %a" Process.pp proc);
             step_process pool scheduler proc
       done
     with Exit -> ());
    Logs.debug (fun f -> f "< exit worker loop")
end

include Scheduler

module Pool = struct
  let get_pool, set_pool = Thread_local.make ~name:"POOL"
  let shutdown pool = pool.stop <- true

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
          Scheduler.run pool scheduler ())
    in
    Logs.debug (fun f -> f "Created %d schedulers" (List.length schedulers));
    (pool, List.map spawn schedulers)
end
