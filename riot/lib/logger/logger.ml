open Runtime
module Scheduler_uid = Core.Scheduler_uid

type opts = { print_source : bool; print_time : bool; color_output : bool }

type ('a, 'b) logger_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type namespace = string list
type level = Debug | Error | Info | Trace | Warn

module Level = struct
  let to_int = function
    | Trace -> 5
    | Debug -> 4
    | Info -> 2
    | Warn -> 1
    | Error -> 0

  let should_log current x =
    match current with
    | None -> false
    | Some log_level -> to_int x <= to_int log_level

  let to_color_string t =
    match t with
    | Error -> "\x1b[31m"
    | Warn -> "\x1b[33m"
    | Debug -> "\x1b[36m"
    | Info -> ""
    | Trace -> "\x1b[35m"

  let pp ppf t =
    match t with
    | Error -> Format.fprintf ppf "error"
    | Warn -> Format.fprintf ppf "warn"
    | Debug -> Format.fprintf ppf "debug"
    | Info -> Format.fprintf ppf "info"
    | Trace -> Format.fprintf ppf "trace"
end

type log = {
  level : level;
  ts : Ptime.t;
  src : Scheduler_uid.t * Core.Pid.t;
  ns : namespace;
  message : string;
}

let __on_log__ : (log -> unit) ref = ref (fun _ -> ())
let set_on_log log = __on_log__ := log
let on_log log = !__on_log__ log

let write : type a. level -> namespace -> (a, unit) logger_format -> unit =
 fun level ns msgf ->
  let ts = Ptime_clock.now () in
  let sch = Scheduler.get_current_scheduler () in
  let pid = self () in
  let src = (sch.uid, pid) in
  let buf = Buffer.create 128 in

  msgf @@ fun fmt ->
  Format.kfprintf
    (fun _ ->
      let message = Buffer.contents buf in
      on_log { ts; level; ns; src; message };

      ())
    (Format.formatter_of_buffer buf)
    (fmt ^^ "%!")

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

let log_level = ref None

module Make (B : Namespace) : Intf = struct
  let set_log_level x = log_level := x

  let debug msgf =
    if Level.should_log !log_level Debug then write Debug B.namespace msgf

  let info msgf =
    if Level.should_log !log_level Info then write Info B.namespace msgf

  let trace msgf =
    if Level.should_log !log_level Trace then write Trace B.namespace msgf

  let warn msgf =
    if Level.should_log !log_level Warn then write Warn B.namespace msgf

  let error msgf =
    if Level.should_log !log_level Error then write Error B.namespace msgf
end

include Make (struct
  let namespace = []
end)
