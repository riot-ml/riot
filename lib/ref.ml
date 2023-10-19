type 'a t = Ref : int64 -> 'a t [@@unboxed]

let __current__ = Atomic.make 0L
let pp ppf (Ref pid) = Format.fprintf ppf "#Ref<%s>" (Int64.to_string pid)

let rec make () =
  let last = Atomic.get __current__ in
  let current = last |> Int64.succ in
  if Atomic.compare_and_set __current__ last current then Ref last else make ()

let equal : type a b. a t -> b t -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Ref a', Ref b' when a' == b' -> Some (Obj.magic Type.Equal)
  | _ -> None

let is_newer (Ref a) (Ref b) = Int64.compare a b = 1
