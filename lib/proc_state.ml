type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let finished x = Finished x
let suspended_with k e = Suspended (k, e)

let handler_continue =
  let retc signal = finished (Ok signal) in
  let exnc exn = finished (Error exn) in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
   fun e -> Some (fun k -> suspended_with k e)
  in
  Effect.Shallow.{ retc; exnc; effc }

let make fn =
  let k = Effect.Shallow.fiber fn in
  Effect.Shallow.continue_with k () handler_continue
