type level = Debug | Info | Trace | Log

let pp_level ppf t =
  match t with
  | Log -> ()
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
    ("%a[thread=%d] @[" ^^ fmt ^^ "@]@.")
    pp_level level domain

let trace _msgf = ()
let debug _msgf = ()
let info msgf = msg Info msgf
let log msgf = msg Log msgf
