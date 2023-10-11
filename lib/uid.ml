type t = int64

let __current__ = Atomic.make 0L

let next () =
  let last = Atomic.get __current__ in
  let current = last |> Int64.succ in
  Atomic.set __current__ current;
  last

let equal a b = Int64.equal a b
let pp ppf t = Format.fprintf ppf "%s" (Int64.to_string t)
