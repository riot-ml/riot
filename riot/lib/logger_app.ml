open Global
include Logger

open Logger.Make (struct
  let namespace = [ "riot"; "logger" ]
end)

module Formatter = struct
  type Message.t += Log of log

  let stdout =
    Format.make_formatter (output_substring stdout) (fun () -> flush stdout)

  let rec formatter_loop config =
    match receive_any () with
    | Log { message; ts; src = sch, pid; level; ns } ->
        let pp_now = Ptime.pp_rfc3339 ~frac_s:5 ~space:true ~tz_offset_s:0 () in

        let ns_str =
          match ns with [] -> "" | _ -> String.concat "." ns ^ "::"
        in

        let buf = Buffer.create 128 in
        let fmt = Format.formatter_of_buffer buf in

        if config.color_output then
          Format.fprintf fmt "%s" (Level.to_color_string level);
        if config.print_time then (
          let parts =
            Format.asprintf "%a" pp_now ts |> String.split_on_char ' '
          in
          let time = List.nth parts 1 in
          Format.fprintf fmt "%s " time;
          if config.print_source then
            Format.fprintf fmt "[thread=%a,pid=%a] " Scheduler_uid.pp sch Pid.pp
              pid;
          Format.fprintf fmt "[%s%a] %s\x1b[0m\n%!" ns_str Level.pp level
            message;

          Format.fprintf fmt "%!";
          Format.printf "%s%!" (Buffer.contents buf);

          formatter_loop config)
    | _ -> formatter_loop config

  let start_link config =
    let pid =
      spawn_link (fun () ->
          Process.flag (Priority High);
          formatter_loop config)
    in
    set_on_log (fun log -> send pid (Log log));
    Ok pid

  let child_spec config = Supervisor.child_spec start_link config
end

let default_opts =
  { print_time = true; print_source = false; color_output = true }

let start () =
  let child_specs = [ Formatter.child_spec default_opts ] in
  Supervisor.start_link ~child_specs ()
