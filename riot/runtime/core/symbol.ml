type 'a t = Symbol : int64 -> 'a t [@@unboxed]

let __current__ = Atomic.make 0L
let pp ppf (Symbol pid) = Format.fprintf ppf "#Symbol<%s>" (Int64.to_string pid)

let rec make () =
  let last = Atomic.get __current__ in
  let current = last |> Int64.succ in
  if Atomic.compare_and_set __current__ last current then Symbol last else make ()

let equal (Symbol a) (Symbol b) = Int64.equal a b

let type_equal : type a b. a t -> b t -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Symbol a', Symbol b' when Int64.equal a' b' -> Some (Obj.magic Type.Equal)
  | _ -> None

let is_newer (Symbol a) (Symbol b) = Int64.compare a b = 1
let hash (Symbol a) = Int64.hash a

module Map = Util.Dashmap.Make (struct
  type key = unit t

  let hash = hash
  let equal = equal
end)
