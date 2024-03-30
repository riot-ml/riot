open Global

open Logger.Make (struct
  let namespace = [ "riot"; "supervisor" ]
end)

type child_spec =
  | Child : {
      initial_state : 'state;
      start_link : 'state -> (Pid.t, [> `Exit of exn ]) result;
    }
      -> child_spec

let child_spec start_link initial_state = Child { start_link; initial_state }

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

let start_child (Child { start_link; initial_state }) =
  start_link initial_state |> Result.get_ok

let init_child spec =
  let pid = start_child spec in
  trace (fun f ->
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
    trace (fun f -> f "child %a is down" Pid.pp pid);
    let spec = List.assoc pid state.children in
    let children = init_child spec :: List.remove_assoc pid state.children in
    `continue { state with children })

type sup_request = List_children_req of { reply : Pid.t; ref : unit Ref.t }

type sup_response =
  | List_children_res of { children : Pid.t list; ref : unit Ref.t }

type Message.t +=
  | Supervisor_request of sup_request
  | Supervisor_response of sup_response

let rec loop state =
  trace (fun f -> f "entered supervision loop");
  let selector : _ Message.selector =
   fun msg ->
    match msg with
    | Process.Messages.Exit (pid, reason) when List.mem_assoc pid state.children
      ->
        `select (`child_exit (pid, reason))
    | Supervisor_request msg -> `select (`req msg)
    | _ -> `skip
  in
  match receive ~selector () with
  | `child_exit (pid, Normal) ->
      trace (fun f -> f "child %a stopped normally" Pid.pp pid);
      let state =
        { state with children = List.remove_assoc pid state.children }
      in
      loop state
  | `child_exit (pid, reason) ->
      trace (fun f ->
          f "child %a stopped: %a" Pid.pp pid Process.pp_reason reason);
      handle_child_exit pid reason state
  | `req (List_children_req { reply; ref }) ->
      let children = List.map (fun (pid, _) -> pid) state.children in
      send reply (Supervisor_response (List_children_res { children; ref }));
      loop state

and handle_child_exit pid _reason state =
  match restart_child pid state with
  | `continue state -> loop state
  | `terminate ->
      trace (fun f ->
          f "Supervisor %a reached max restarts of %d" Pid.pp (self ())
            state.restart_limit)

let start_supervisor state =
  trace (fun f ->
      f "Initializing supervisor %a with %d child specs" Pid.pp (self ())
        (List.length state.child_specs));
  Process.flag (Trap_exit true);
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

let children pid =
  let ref = Ref.make () in
  send pid (Supervisor_request (List_children_req { reply = self (); ref }));
  let selector msg =
    match msg with
    | Supervisor_response (List_children_res { ref = ref'; _ } as msg)
      when Ref.equal ref ref' ->
        `select msg
    | _ -> `skip
  in
  match receive ~selector () with
  | List_children_res { children; _ } -> children
