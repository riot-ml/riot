type t = int

let zero : t = 0
let __current__ = Atomic.make 0
let next () = Atomic.fetch_and_add __current__ 1
let equal a b = Int.equal a b
let pp ppf pid = Format.fprintf ppf "<0.%d.0>" pid

let reset () =
  Logs.debug (fun f -> f "Resetting Process Ids");
  Atomic.set __current__ 0
