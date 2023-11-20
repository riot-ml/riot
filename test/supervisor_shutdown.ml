[@@@warning "-8"]

open Riot

type Message.t += Ping_me of Pid.t

module Ping = struct
  let loop reply =
    send reply (Ping_me (self ()));
    let rec loop () =
      yield ();
      loop ()
    in
    loop ()

  let start_link n =
    let pid = spawn_link (fun () -> loop n) in
    Ok pid
end

let main () =
  process_flag (Trap_exit true);
  let this = self () in
  let sup =
    Supervisor.start_link ~restart_limit:2
      ~child_specs:[ Supervisor.child_spec ~start_link:Ping.start_link this ]
      ()
    |> Result.get_ok
  in

  let (Ping_me child_pid) = receive () in
  Logger.debug (fun f -> f "received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  let (Ping_me child_pid) = receive () in
  Logger.debug (fun f -> f "received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  let (Ping_me child_pid) = receive () in
  Logger.debug (fun f -> f "received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  match receive () with
  | Process.Messages.Exit (pid, _reason) when Pid.equal pid sup ->
      Logger.info (fun f ->
          f "supervisor_shutdown_test: supervisor finished as expected");
      sleep 0.001;
      shutdown ()
  | _ -> failwith "supervisor_shutdown_test: expected supervisor failure"

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
