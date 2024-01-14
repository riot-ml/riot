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
  let _ = Logger.start () in
  Logger.set_log_level (Some Info);
  sleep 0.1;
  process_flag (Trap_exit true);
  let this = self () in
  let sup =
    Supervisor.start_link ~restart_limit:2
      ~child_specs:[ Supervisor.child_spec Ping.start_link this ]
      ()
    |> Result.get_ok
  in

  let (Ping_me child_pid) = receive ~after:500_000L () in
  Logger.debug (fun f -> f "#1 received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  let (Ping_me child_pid) = receive ~after:500_000L () in
  Logger.debug (fun f -> f "#2 received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  let (Ping_me child_pid) = receive ~after:500_000L () in
  Logger.debug (fun f -> f "#3 received pid %a" Pid.pp child_pid);

  exit child_pid Process.Exit_signal;

  match receive ~after:500_000L () with
  | Process.Messages.Exit (pid, _reason) when Pid.equal pid sup ->
      Logger.info (fun f ->
          f "supervisor_shutdown_test: supervisor finished as expected");

      shutdown ()
  | _ -> failwith "supervisor_shutdown_test: expected supervisor failure"

let () = Riot.run @@ main
