include Riot_runtime.Import

(* TODO(@leostera): move these into the Runtime module below *)
include Riot_runtime.Core.Process.Exn
include Riot_runtime.Core.Proc_registry.Exn

let ( let* ) = Result.bind
