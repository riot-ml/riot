type t = int64

let hash = Int64.hash
let of_int t = t
let to_int t = t
let equal = Int64.equal
let pp fmt t = Format.fprintf fmt "Token(%Ld)" t

let next =
  let current = ref 0L in
  fun () ->
    current := Int64.add !current 1L;
    !current
