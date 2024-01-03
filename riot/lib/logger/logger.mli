type opts = { print_source : bool; print_time : bool; color_output : bool }

type ('a, 'b) logger_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type namespace = string list
type level = Debug | Error | Info | Trace | Warn

val set_log_level : level option -> unit

module Level : sig
  val to_int : level -> int
  val should_log : level option -> level -> bool
  val to_color_string : level -> string
  val pp : Format.formatter -> level -> unit
end

type log = {
  level : level;
  ts : Ptime.t;
  src : Core.Scheduler_uid.t * Core.Pid.t;
  ns : namespace;
  message : string;
}

val set_on_log : (log -> unit) -> unit

module type Intf = sig
  val set_log_level : level option -> unit
  val debug : ('a, unit) logger_format -> unit
  val error : ('a, unit) logger_format -> unit
  val info : ('a, unit) logger_format -> unit
  val trace : ('a, unit) logger_format -> unit
  val warn : ('a, unit) logger_format -> unit
end

module type Namespace = sig
  val namespace : namespace
end

module Make : functor (_ : Namespace) -> Intf

val debug : ('a, unit) logger_format -> unit
val error : ('a, unit) logger_format -> unit
val info : ('a, unit) logger_format -> unit
val trace : ('a, unit) logger_format -> unit
val warn : ('a, unit) logger_format -> unit
