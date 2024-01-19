open Global

let set_log_level = Runtime.Log.set_log_level

let syscalls () =
  let pool = _get_pool () in
  ( pool.io_scheduler.calls_accept,
    pool.io_scheduler.calls_receive,
    pool.io_scheduler.calls_send,
    pool.io_scheduler.calls_connect )

module Stats = struct
open Logger.Make(struct
  let namespace = ["riot"; "runtime"; "stats"]
end)

  type Message.t += Print_stats

  let mb b = Int.to_float b /. 1024.0 /. 1024.0

  let print_gc_stats () =
    let stat = Gc.stat () in
    info (fun f ->
        f
          {|gc_stats:
live_bytes=%f mb in %d blocks
free_bytes=%f mb in %d blocks
heap_bytes=%f mb in %d chunks
fragments=%d
compactions=%d
|}
          (stat.live_words * 8 |> mb)
          stat.live_blocks
          (stat.free_words * 8 |> mb)
          stat.free_blocks
          (stat.heap_words * 8 |> mb)
          stat.heap_chunks stat.fragments stat.compactions)

  let rec loop () =
    print_gc_stats ();
    receive () |> ignore;
    loop ()

  let start ?(every = 2_000_000L) () =
    let stats = spawn loop in
    Timer.send_interval stats Print_stats ~every |> ignore
end
