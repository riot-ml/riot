open Core

module Timer : sig
  type t

  val pp : Format.formatter -> t -> unit
  val make : float -> [ `interval | `one_off ] -> (unit -> unit) -> t
  val equal : t -> t -> bool
end

type t

val create : unit -> t

val make_timer :
  t -> float -> [ `interval | `one_off ] -> (unit -> unit) -> unit Ref.t

val ends_at : Ptime.t -> Ptime.span -> Ptime.t
val tick : t -> unit
