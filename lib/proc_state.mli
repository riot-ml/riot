type ('a, 'b) continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t

val make : (unit -> 'a) -> 'a t
(** Create a [Proc_state.t] which will internally represent the state of a
    process by lifting the process' body function into a
    [Effect.Shallow.continuation]
*)
