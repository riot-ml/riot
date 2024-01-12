open Global

let set_log_level = Runtime.Log.set_log_level

let syscalls () =
  let pool = _get_pool () in
  ( pool.io_scheduler.calls_accept,
    pool.io_scheduler.calls_receive,
    pool.io_scheduler.calls_send,
    pool.io_scheduler.calls_connect )

module Stats = struct
  open Logger.Make (struct
    let namespace = [ "riot"; "runtime"; "stats" ]
  end)

  type Message.t += Print_stats

  let mb b = Int.to_float b /. 1024.0 /. 1024.0

  let print_scheduler_stats () =
    let pool = _get_pool () in
    let total_processes = pool.proc_count in
    let processes = processes () |> List.of_seq in
    let live_process_count = processes |> List.length in
    let total_schedulers = pool.schedulers |> List.length in
    let breakdown =
      pool.schedulers
      |> List.map (fun (sch : Scheduler.t) ->
             Format.asprintf "  sch #%a [live_procs=%d; timers=%d]"
               Runtime.Core.Scheduler_uid.pp sch.uid
               (Runtime.Core.Proc_queue.size sch.run_queue)
               (Runtime.Time.Timer_wheel.size sch.timers))
      |> String.concat "\n"
    in
    info (fun f ->
        f
          {|pool: 

live_processes: %d
total_processes: %d
total_schedulers: %d
%s
|}
          live_process_count total_processes total_schedulers breakdown)

  let print_gc_stats () =
    let stat = Gc.stat () in
    info (fun f ->
        f
          {|gc_stats:
live_bytes=%f mb in %d blocks
free_bytes=%f mb in %d blocks
heap_bytes=%f mb in %d chunks
max_heap_size=%f mb
fragments=%d
compactions=%d
|}
          (stat.live_words * 8 |> mb)
          stat.live_blocks
          (stat.free_words * 8 |> mb)
          stat.free_blocks
          (stat.heap_words * 8 |> mb)
          stat.heap_chunks
          (stat.top_heap_words * 8 |> mb)
          stat.fragments stat.compactions)

  let rec loop () =
    print_scheduler_stats ();
    print_gc_stats ();
    receive () |> ignore;
    loop ()

  let start ?(every = 2_000_000L) () =
    let stats = spawn loop in
    Timer.send_interval stats Print_stats ~every |> ignore
end
