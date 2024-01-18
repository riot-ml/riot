open Global

open Logger.Make (struct
  let namespace = [ "riot"; "dynamic_supervisor" ]
end)

type state = { max_children : int; curr_children : int; name : string }

type Message.t +=
  | Start_child of Pid.t * Supervisor.child_spec
  | Started_child of Pid.t
  | Max_children

let rec loop state =
  match receive () with
  | Process.Messages.Monitor _ ->
      trace (fun f -> f "child finished");
      loop { state with curr_children = Int.max 0 (state.curr_children - 1) }
  | Start_child (reply, spec) -> handle_start_child state reply spec
  | _ -> loop state

and handle_start_child state reply child_spec =
  let curr_children = state.curr_children + 1 in
  if curr_children < state.max_children then (
    let pid = Supervisor.start_child child_spec in
    Process.monitor pid;
    trace (fun f -> f "started child %d" curr_children);
    send reply (Started_child pid);
    loop { state with curr_children })
  else (
    send reply Max_children;
    loop state)

let init ({ max_children; name; _ } as state) =
  register name (self ());
  Process.flag (Trap_exit true);
  trace (fun f -> f "max %d children" max_children);
  loop state

let start_link state =
  let pid = spawn_link (fun () -> init state) in
  Ok pid

let child_spec ?(max_children = 50) ~name () =
  let state = { max_children; curr_children = 0; name } in
  Supervisor.child_spec start_link state

let start_child pid spec =
  let ref = Ref.make () in
  send pid (Start_child (self (), spec));
  match[@warning "-8"] receive ~ref () with
  | Started_child pid -> Ok pid
  | Max_children -> Error `Max_children
