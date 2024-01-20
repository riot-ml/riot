exception Unwind

type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let is_finished x = match x with Finished _ -> true | _ -> false

type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform : 'a Effect.t -> 'a step
  | Delay : 'a step
  | Suspend : 'a step
  | Yield : unit step
  | Terminate : 'a step

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

let run : type a. reductions:int -> perform:perform -> a t -> a t option =
 fun ~reductions ~perform t ->
  let exception Yield of a t in
  let exception Unwind in
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
            | Terminate ->
                ignore (discontinue_with fn Unwind);
                raise Unwind
          in
          t := perform.perform (k fn) e
    done;
    Some !t
  with
  | Yield t -> Some t
  | Unwind -> None

let drop k exn id =
  let retc _signal =
    Log.debug (fun f -> f "dropping continuation return: %s" id);
    ()
  in
  let exnc _exn =
    Log.debug (fun f -> f "dropping continuation exception: %s" id);
    ()
  in
  let effc _eff =
    Log.debug (fun f -> f "dropping continuation effect: %s" id);
    None
  in
  let handler = Effect.Shallow.{ retc; exnc; effc } in
  Effect.Shallow.discontinue_with k exn handler

let unwind ~id (t : 'a t) =
  match t with
  | Finished result -> ignore result
  | Suspended (k, _) -> ignore (drop k Unwind id)
  | Unhandled (k, _) -> ignore (drop k Unwind id)
