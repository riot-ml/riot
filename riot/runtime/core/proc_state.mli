type ('a, 'b) continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform : 'a Effect.t -> 'a step
  | Delay : 'a step
  | Yield : unit step

type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform : 'a 'b. ('a, 'b) step_callback } [@@unboxed]

val make : ('a -> 'b) -> 'a Effect.t -> 'b t
val run : perform:perform -> 'a t -> 'a t
