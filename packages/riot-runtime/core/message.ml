type t = ..
type 'msg selector = t -> [ `select of 'msg | `skip ]
type envelope = { msg : t; uid : unit Ref.t }

let envelope msg = { uid = Ref.make (); msg }
