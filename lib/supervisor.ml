open Riot_api

type child_spec =
  | Child : {
      initial_state : 'state;
      start_link : 'state -> (Pid.t, [> `Exit of exn ]) result;
    }
      -> child_spec

let child_spec ~start_link initial_state = Child { start_link; initial_state }

type strategy = One_for_one | One_for_all | Rest_for_one | Simple_one_for_one
type timestamp = float

type state = {
  strategy : strategy;
  restart_limit : int;
  restart_period : int;
  child_specs : child_spec list;
  children : (Pid.t * child_spec) list;
  restarts : timestamp list;
}
[@@warning "-69"]

let init_child spec =
  let (Child { start_link; initial_state }) = spec in
  let pid = start_link initial_state |> Result.get_ok in
  Logs.info (fun f ->
      let this = self () in
      f "Supervisor %a started child %a" Pid.pp this Pid.pp pid);
  (pid, spec)

let init_children state = List.map init_child state.child_specs

let add_restart state =
  let now = Unix.gettimeofday () in
  { state with restarts = now :: state.restarts }

let max_restarts_reached state =
  List.length state.restarts > state.restart_limit

let restart_child pid state =
  let state = add_restart state in
  if max_restarts_reached state then `terminate
  else (
    Logs.info (fun f -> f "child %a is down" Pid.pp pid);
    let spec = List.assoc pid state.children in
    let children = init_child spec :: List.remove_assoc pid state.children in
    `continue { state with children })

let rec loop state =
  Logs.debug (fun f -> f "supervisor loop");
  match receive () with
  | Process.Messages.Exit (pid, Normal) when List.mem_assoc pid state.children
    ->
      Logs.info (fun f -> f "child %a stopped normally" Pid.pp pid);
      let state =
        { state with children = List.remove_assoc pid state.children }
      in
      loop state
  | Process.Messages.Exit (pid, reason) when List.mem_assoc pid state.children
    ->
      Logs.info (fun f ->
          f "child %a stopped: %a" Pid.pp pid Process.pp_reason reason);
      handle_child_exit pid reason state
  | _ -> loop state

and handle_child_exit pid _reason state =
  match restart_child pid state with
  | `continue state -> loop state
  | `terminate ->
      Logs.info (fun f ->
          f "Supervisor %a reached max restarts of %d" Pid.pp (self ())
            state.restart_limit)

let start_supervisor state =
  Logs.info (fun f ->
      f "Initializing supervisor %a with %d child specs" Pid.pp (self ())
        (List.length state.child_specs));
  process_flag (Trap_exit true);
  let state = { state with children = init_children state } in
  loop state

let start_link ?(strategy = One_for_one) ?(restart_limit = 1)
    ?(restart_period = 5) ~child_specs () =
  let state =
    {
      strategy;
      restart_limit;
      restart_period;
      child_specs;
      children = [];
      restarts = [];
    }
  in
  let sup_pid = spawn_link (fun () -> start_supervisor state) in
  Ok sup_pid
