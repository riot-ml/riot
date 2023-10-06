type level = Debug | Info | Trace | Log

let level_to_int = function Trace -> 5 | Debug -> 4 | Info -> 2 | Log -> 1
let log_level = ref (Some Debug)
let set_log_level x = log_level := x

let should_log x =
  match !log_level with
  | None -> false
  | Some log_level -> level_to_int x <= level_to_int log_level

let pp_level ppf t =
  match t with
  | Log -> ()
  | Debug -> Format.fprintf ppf "DEBUG "
  | Info -> Format.fprintf ppf "INFO  "
  | Trace -> Format.fprintf ppf "TRACE "

type ('a, 'b) message_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

let log_lock = Mutex.create ()

let stdout =
  Format.make_formatter (output_substring stdout) (fun () -> flush stdout)

let msg : type a. level -> (a, unit) message_format -> unit =
 fun level msgf ->
  msgf @@ fun fmt ->
  Mutex.lock log_lock;
  let domain = (Domain.self () :> int) in
  Format.kfprintf
    (fun _ -> Mutex.unlock log_lock)
    stdout
    ("%a %a[thread=%d] @[" ^^ fmt ^^ "@]@.")
    (Ptime.pp_rfc3339 ~frac_s:5 ~space:true ~tz_offset_s:0 ())
    (Ptime_clock.now ()) pp_level level domain

let trace msgf = if should_log Trace then msg Trace msgf
let debug msgf = if should_log Debug then msg Debug msgf
let info msgf = if should_log Info then msg Info msgf
let log msgf = msg Log msgf
