type t = int

let __current__ = Atomic.make 0
let next () = Atomic.fetch_and_add __current__ 1
let equal a b = Int.equal a b
let pp ppf pid = Format.fprintf ppf "%d" pid
