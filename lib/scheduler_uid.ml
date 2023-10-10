type t = int

let __current__ = Atomic.make 0
let next () = Atomic.fetch_and_add __current__ 1
let pp ppf t = Format.fprintf ppf "%d" t

let reset () =
  Logs.debug (fun f -> f "Resetting Scheduler Uids");
  Atomic.set __current__ 0
