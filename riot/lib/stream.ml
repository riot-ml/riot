include Stdlib.Seq

type 'v t = 'v Seq.t

let next = uncons
let unfold fn data = Seq.unfold (fun data -> fn data) data

type 'acc control_flow = [ `continue of 'acc | `halt of 'acc ]

let rec reduce_while init fn t =
  match t () with
  | Seq.Nil -> init
  | Seq.Cons (v, t') -> (
      match fn v init with
      | `continue acc -> reduce_while acc fn t'
      | `halt acc -> acc)
