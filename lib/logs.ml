(** Low-level mutex-coordinated logs for the Riot engine. 
    These are super slow, and are intended for usage within the engine alone.

    If you're looking for logs for your application, look into
    {!module:Riot.Logger} instead.
*)

type level = Debug | Error | Info | Trace | Warn

let level_to_int = function
  | Trace -> 5
  | Debug -> 4
  | Info -> 2
  | Warn -> 1
  | Error -> 0

let log_level = ref (Some Error)
let set_log_level x = log_level := x

let should_log x =
  match !log_level with
  | None -> false
  | Some log_level -> level_to_int x <= level_to_int log_level

let pp_level ppf t =
  match t with
  | Error -> Format.fprintf ppf "ERROR "
  | Warn -> Format.fprintf ppf "WARN "
  | Debug -> Format.fprintf ppf "DEBUG "
  | Info -> Format.fprintf ppf "INFO "
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
let warn msgf = if should_log Warn then msg Warn msgf
let error msgf = if should_log Error then msg Error msgf
