type t = { _id : int64 } [@@unboxed]

let pp ppf pid = Format.fprintf ppf "<0.%s.0>" (Int64.to_string pid._id)
let make _id = { _id }
let zero : t = make 0L
let __current__ = Atomic.make 1L

let rec next () =
  let last = Atomic.get __current__ in
  let current = last |> Int64.succ in
  if Atomic.compare_and_set __current__ last current then make last else next ()

let equal a b = Int64.equal a._id b._id
let compare a b = Int64.compare a._id b._id
let hash t = Int64.hash t._id

let reset () =
  Log.debug (fun f -> f "Resetting Process Ids");
  Atomic.set __current__ 1L
