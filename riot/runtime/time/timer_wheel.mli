open Core

module Timer : sig
  type t

  val pp : Format.formatter -> t -> unit
  val make : int64 -> [ `interval | `one_off ] -> (unit -> unit) -> t
  val equal : t -> t -> bool
end

type t

val create : unit -> t
val is_finished : t -> unit Symbol.t -> bool
val remove_timer : t -> unit Symbol.t -> unit

val make_timer :
  t -> int64 -> [ `interval | `one_off ] -> (unit -> unit) -> unit Symbol.t

val clear_timer : t -> unit Symbol.t -> unit
val tick : t -> unit
val can_tick : t -> bool
