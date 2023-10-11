(* TODO(leostera): make refs a uuid or something larger than int *)

type 'a t = Ref : int -> 'a t

let __current__ = Atomic.make 0
let pp ppf (Ref pid) = Format.fprintf ppf "#Ref<%d>" pid
let make () = Ref (Atomic.fetch_and_add __current__ 1)

let equal : type a b. a t -> b t -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Ref a', Ref b' when a' == b' -> Some (Obj.magic Type.Equal)
  | _ -> None
