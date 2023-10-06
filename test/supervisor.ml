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

  let child_pid =
    match receive () with
    | Ping_me pid ->
        Logs.info (fun f -> f "%a received pid %a" Pid.pp this Pid.pp pid);
        pid
    | _ -> failwith "expected child pid"
  in

  exit child_pid Normal;

  let (Ping_me child_pid) = receive () in

  exit child_pid Normal;

  let (Ping_me child_pid) = receive () in

  exit child_pid Normal;

  match receive () with
  | Message.Exit pid when Pid.equal pid sup ->
      Logs.log (fun f -> f "supervisor finished as expected");
      shutdown ()
  | _ -> failwith "expected supervisor failure"

let () =
  Logs.set_log_level None;
  Riot.run @@ main
