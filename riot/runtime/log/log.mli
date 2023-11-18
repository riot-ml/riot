type level = Debug | Error | Info | Trace | Warn

val set_log_level : level option -> unit

type ('a, 'b) message_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

val trace : ('a, unit) message_format -> unit
val debug : ('a, unit) message_format -> unit
val info : ('a, unit) message_format -> unit
val warn : ('a, unit) message_format -> unit
val error : ('a, unit) message_format -> unit
