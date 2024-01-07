type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform : 'a Effect.t -> 'a step
  | Delay : 'a step
  | Suspend : 'a step
  | Yield : unit step

type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform : 'a 'b. ('a, 'b) step_callback } [@@unboxed]

let pp fmt t =
  match t with
  | Finished (Ok _) -> Format.fprintf fmt "Finished(Ok _)"
  | Finished (Error exn) ->
      Format.fprintf fmt "Finished(Error %s)" (Printexc.to_string exn)
  | Suspended (_, _) -> Format.fprintf fmt "Suspended"
  | Unhandled (_, _) -> Format.fprintf fmt "Unhandled"

let finished x = Finished x
let suspended_with k e = Suspended (k, e)

let handler_continue =
  let retc signal = finished (Ok signal) in
  let exnc exn = finished (Error exn) in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
   fun e -> Some (fun k -> suspended_with k e)
  in
  Effect.Shallow.{ retc; exnc; effc }

let continue_with k v = Effect.Shallow.continue_with k v handler_continue

let discontinue_with k exn =
  Effect.Shallow.discontinue_with k exn handler_continue

let unhandled_with k v = Unhandled (k, v)

let make fn eff =
  let k = Effect.Shallow.fiber fn in
  Suspended (k, eff)

let run : type a. reductions:int -> perform:perform -> a t -> a t =
 fun ~reductions ~perform t ->
  let exception Yield of a t in
  let reductions = ref reductions in
  let t = ref t in
  try
    while true do
      if !reductions = 0 then raise_notrace (Yield !t);
      reductions := !reductions - 1;
      match !t with
      | Finished _ as finished -> raise_notrace (Yield finished)
      | Unhandled (fn, v) -> raise_notrace (Yield (continue_with fn v))
      | Suspended (fn, e) as suspended ->
          let k : type c. (c, a) continuation -> c step -> a t =
           fun fn step ->
            match step with
            | Delay -> suspended
            | Continue v -> continue_with fn v
            | Discontinue exn -> discontinue_with fn exn
            | Reperform eff -> unhandled_with fn (Effect.perform eff)
            | Yield -> raise_notrace (Yield (continue_with fn ()))
            | Suspend -> raise_notrace (Yield suspended)
          in
          t := perform.perform (k fn) e
    done;
    !t
  with Yield t -> t
