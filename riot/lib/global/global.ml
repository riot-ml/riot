include Runtime.Import
include Runtime.Core.Process.Exn
include Runtime.Core.Proc_registry.Exn

module Runtime = struct
  let set_log_level = Runtime.Log.set_log_level
end
