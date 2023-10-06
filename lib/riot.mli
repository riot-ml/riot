module Logs = Logs

module Pid : sig
  type t

  val zero : t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Message : sig
  type select_marker = Take | Skip
  type monitor = Process_down of Pid.t
  type t = ..
  type t += Exit_signal | Monitor of monitor
end

module Process : sig
  type t
end

type exit_reason =
  | Normal
  | Exit_signal
  | Timeout_value
  | Bad_link
  | Exception of exn

type signal = Link | Unlink | Exit | Monitor | Demonitor | Message

val yield : unit -> unit
val self : unit -> Pid.t
val exit : Pid.t -> exit_reason -> unit
val send : Pid.t -> Message.t -> unit
val spawn : (unit -> unit) -> Pid.t

exception Link_no_process of Pid.t

val link : Pid.t -> unit
val monitor : Pid.t -> Pid.t -> unit
val processes : unit -> (Pid.t * Process.t) Seq.t
val is_process_alive : Pid.t -> bool
val random : unit -> Random.State.t
val receive : ?select:(Message.t -> Message.select_marker) -> unit -> Message.t
val shutdown : unit -> unit
val run : ?rnd:Random.State.t -> ?workers:int -> (unit -> unit) -> unit
