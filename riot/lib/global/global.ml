include Runtime.Import
include Runtime.Core.Process.Exn
include Runtime.Core.Proc_registry.Exn

module Runtime = struct
  let set_log_level = Runtime.Log.set_log_level

  let syscalls () =
    let pool = _get_pool () in
    ( pool.io_scheduler.calls_accept,
      pool.io_scheduler.calls_receive,
      pool.io_scheduler.calls_send,
      pool.io_scheduler.calls_connect )
end

let ( let* ) = Result.bind
