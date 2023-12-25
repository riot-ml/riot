open Runtime
include Logger

module Formatter = struct
  type Message.t += Log of log [@@unboxed]

  let stdout =
    Format.make_formatter (output_substring stdout) (fun () -> flush stdout)

  let rec formatter_loop config =
    match receive () with
    | Log { message; ts; src = sch, pid; level; ns } ->
        let pp_now = Ptime.pp_rfc3339 ~frac_s:5 ~space:true ~tz_offset_s:0 () in

        let ns_str =
          match ns with [] -> "" | _ -> String.concat "." ns ^ "::"
        in

        if config.color_output then
          Format.fprintf stdout "%s" (Level.to_color_string level);
        if config.print_time then (
          let parts =
            Format.asprintf "%a" pp_now ts |> String.split_on_char ' '
          in
          let time = List.nth parts 1 in
          Format.fprintf stdout "%s" time;
          if config.print_source then
            Format.fprintf stdout "[thread=%a,pid=%a] " Scheduler_uid.pp sch
              Pid.pp pid;
          Format.fprintf stdout "[%s%a] %s\x1b[0m\n%!" ns_str Level.pp level
            message;

          formatter_loop config)
    | _ -> formatter_loop config

  let start_link config =
    let pid = spawn_link (fun () -> formatter_loop config) in
    set_on_log (fun log -> send pid (Log log));
    Ok pid

  let child_spec config = Supervisor.child_spec ~start_link config
end

let name = "Riot.Logger"

let default_opts =
  { print_time = true; print_source = false; color_output = true }

let start () =
  let child_specs = [ Formatter.child_spec default_opts ] in
  let (Ok pid) = Supervisor.start_link ~child_specs () in
  Ok pid
[@@warning "-8"]
