include Runtime.Import

(* TODO(@leostera): move these into the Runtime module below *)
include Runtime.Core.Process.Exn
include Runtime.Core.Proc_registry.Exn

let ( let* ) = Result.bind
