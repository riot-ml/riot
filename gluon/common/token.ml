type t = int

let of_int t = t
let to_int t = t
let equal = Int.equal
let pp fmt t = Format.fprintf fmt "Token(%d)" t

let next =
  let current = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add current 1
