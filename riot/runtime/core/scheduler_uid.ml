type t = int

let __current__ = Atomic.make 0

let rec next () =
  let last = Atomic.get __current__ in
  let current = Int.succ last in
  if Atomic.compare_and_set __current__ last current then last else next ()

let equal a b = Int.equal a b
let pp ppf t = Format.fprintf ppf "%02d" t
let to_int t = t

let reset () =
  Logs.debug (fun f -> f "Resetting Scheduler Uids");
  Atomic.set __current__ 0
