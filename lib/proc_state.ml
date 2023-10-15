type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let pp ppf t =
  match t with
  | Finished _ -> Format.fprintf ppf "Finished"
  | Suspended (_, _) -> Format.fprintf ppf "Suspended"
  | Unhandled (_, _) -> Format.fprintf ppf "Unhandled"

type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform : 'a Effect.t -> 'a step
  | Delay : 'a step
  | Yield : unit step

let pp_step : type a. Format.formatter -> a step -> unit =
 fun ppf step ->
  match step with
  | Continue _ -> Format.fprintf ppf "Continue"
  | Discontinue _ -> Format.fprintf ppf "Discontinue"
  | Reperform _ -> Format.fprintf ppf "Reperform"
  | Delay -> Format.fprintf ppf "Delay"
  | Yield -> Format.fprintf ppf "Yield"

type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform : 'a 'b. ('a, 'b) step_callback } [@@unboxed]

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

let handler_discontinue exn =
  let retc _ = finished (Error exn) in
  let exnc = retc in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
   fun _ -> Some retc
  in
  Effect.Shallow.{ retc; exnc; effc }

let discontinue_with k exn =
  Effect.Shallow.discontinue_with k exn (handler_discontinue exn)

let unhandled_with k v = Unhandled (k, v)

let make fn eff =
  let k = Effect.Shallow.fiber fn in
  Suspended (k, eff)

(* NOTE(leostera): this behaves like Miou's `once` *)
let run : type a. perform:perform -> a t -> a t =
 fun ~perform t ->
  match t with
  | Finished _ as finished -> finished
  | Unhandled (fn, v) -> continue_with fn v
  | Suspended (fn, e) as suspended ->
      let k : type c. (c, a) continuation -> c step -> a t =
       fun fn step ->
        match step with
        | Delay -> suspended
        | Continue v -> continue_with fn v
        | Discontinue exn -> discontinue_with fn exn
        | Reperform eff -> unhandled_with fn (Effect.perform eff)
        | Yield -> continue_with fn ()
      in
      perform.perform (k fn) e
