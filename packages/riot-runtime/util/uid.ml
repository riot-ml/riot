type t = int64

let __current__ = Atomic.make 0L

let rec next () =
  let last = Atomic.get __current__ in
  let current = Int64.succ last in
  if Atomic.compare_and_set __current__ last current then last else next ()

let equal a b = Int64.equal a b
let pp ppf t = Format.fprintf ppf "%s" (Int64.to_string t)
