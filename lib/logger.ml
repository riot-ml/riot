open Riot_api

type config = { print_source : bool; print_time : bool; color_output : bool }

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

module type Intf = sig
  val set_log_level : level option -> unit
  val debug : ('a, unit) logger_format -> unit
  val error : ('a, unit) logger_format -> unit
  val info : ('a, unit) logger_format -> unit
  val trace : ('a, unit) logger_format -> unit
  val warn : ('a, unit) logger_format -> unit
end

module Logger = struct
  type Message.t +=
    | Log of {
        level : level;
        ts : Ptime.t;
        src : Scheduler_uid.t * Pid.t;
        ns : namespace;
        message : string;
      }
    [@@unboxed]

  module Formatter = struct
    let stdout =
      Format.make_formatter (output_substring stdout) (fun () -> flush stdout)

    let rec formatter_loop config =
      match receive () with
      | Log { message; ts; src = sch, pid; level; ns } ->
          let pp_now =
            Ptime.pp_rfc3339 ~frac_s:5 ~space:true ~tz_offset_s:0 ()
          in

          let ns_str =
            match ns with [] -> "" | _ -> String.concat "." ns ^ "::"
          in

          if config.color_output then
            Format.fprintf stdout "%s" (Level.to_color_string level);
          if config.print_time then Format.fprintf stdout "%a " pp_now ts;
          if config.print_source then
            Format.fprintf stdout "[thread=%a,pid=%a] " Scheduler_uid.pp sch
              Pid.pp pid;
          Format.fprintf stdout "[%s%a] %s\x1b[0m\n%!" ns_str Level.pp level
            message;

          formatter_loop config
      | _ -> formatter_loop config

    let __main_formatter_ : Pid.t ref = ref Pid.zero

    let start_link config =
      let pid = spawn_link (fun () -> formatter_loop config) in
      __main_formatter_ := pid;
      Ok pid

    let child_spec config = Supervisor.child_spec ~start_link config

    let write : type a. level -> namespace -> (a, unit) logger_format -> unit =
     fun level ns msgf ->
      let ts = Ptime_clock.now () in
      let domain = (Domain.self () :> int) in
      let pid = self () in
      let src = (domain, pid) in
      let buf = Buffer.create 128 in

      msgf @@ fun fmt ->
      Format.kfprintf
        (fun _ ->
          let message = Buffer.contents buf in
          Logs.info (fun f -> f "%a send message: %s" Pid.pp pid message);
          send !__main_formatter_ (Log { ts; level; ns; src; message });

          ())
        (Format.formatter_of_buffer buf)
        (fmt ^^ "%!")
  end

  let start_link config =
    Logs.info (fun f -> f "Starting logger...");
    let child_specs = [ Formatter.child_spec config ] in
    Supervisor.start_link ~child_specs ()
end

module type Namespace = sig
  val namespace : namespace
end

module Make (B : Namespace) : Intf = struct
  let log_level = ref (Some Info)
  let set_log_level x = log_level := x

  let debug msgf =
    if Level.should_log !log_level Debug then
      Logger.Formatter.write Debug B.namespace msgf

  let info msgf =
    if Level.should_log !log_level Info then
      Logger.Formatter.write Info B.namespace msgf

  let trace msgf =
    if Level.should_log !log_level Trace then
      Logger.Formatter.write Trace B.namespace msgf

  let warn msgf =
    if Level.should_log !log_level Warn then
      Logger.Formatter.write Warn B.namespace msgf

  let error msgf =
    if Level.should_log !log_level Error then
      Logger.Formatter.write Error B.namespace msgf
end

include Make (struct
  let namespace = []
end)

let start ?(print_time = false) ?(print_source = false) ?(color_output = true)
    () =
  let state = { print_time; print_source; color_output } in
  Logger.start_link state |> Result.map (fun _ -> ())
