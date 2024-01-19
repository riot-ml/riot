module Exn = struct
  exception Receive_timeout
  exception Syscall_timeout
end

type exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

module Messages = struct
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason | Timeout
end

type state =
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      syscall : string;
      token : Gluon.Token.t;
      source : Gluon.Source.t;
    }
  | Running
  | Exited of exit_reason
  | Finalized

type priority = High | Normal | Low

let priority_to_string = function
  | High -> "High"
  | Normal -> "Normal"
  | Low -> "Low"

type process_flags = {
  trap_exits : bool Atomic.t;
  priority : priority Atomic.t;
}

type process_flag = Trap_exit of bool | Priority of priority

let default_flags () =
  { trap_exits = Atomic.make false; priority = Atomic.make Normal }

type t = {
  pid : Pid.t;
  sid : Scheduler_uid.t;
  flags : process_flags;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t option;
  mutable fn : (unit -> exit_reason) option;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable read_save_queue : bool;
      (** the save queue is a temporary queue used for storing messages during a selective receive *)
  links : Pid.t list Atomic.t;
  monitors : unit Pid.Map.t;
  monitored_by : unit Pid.Map.t;
  ready_tokens : (Gluon.Token.t * Gluon.Source.t) Util.Lf_queue.t;
  recv_timeout : unit Ref.t option Atomic.t;
  syscall_timeout : unit Ref.t option Atomic.t;
}
(** The process descriptor. *)

let make sid fn =
  let pid = Pid.next () in
  let proc =
    {
      pid;
      sid;
      cont = None;
      fn = Some fn;
      state = Atomic.make Uninitialized;
      links = Atomic.make [];
      monitors = Pid.Map.create ~size:0 ();
      monitored_by = Pid.Map.create ~size:0 ();
      mailbox = Mailbox.create ();
      save_queue = Mailbox.create ();
      read_save_queue = false;
      flags = default_flags ();
      ready_tokens = Util.Lf_queue.create ();
      recv_timeout = Atomic.make None;
      syscall_timeout = Atomic.make None;
    }
  in
  Log.debug (fun f ->
      f "Making process with pid=%a and size=%d" Pid.pp pid
        (Obj.repr proc |> Obj.reachable_words));
  proc

let init t =
  let fn = t.fn |> Option.get in
  t.cont <- Some (Proc_state.make fn Proc_effect.Yield);
  t.fn <- None;
  Atomic.set t.state Runnable

let free t =
  Atomic.set t.state Finalized;
  Atomic.set t.links [];
  Atomic.set t.recv_timeout None;
  Atomic.set t.syscall_timeout None;
  t.cont <- None;
  t.fn <- None;
  ()

let rec pp ppf t =
  Format.fprintf ppf "Process %a { state = %a; messages = %d; flags = %a }"
    Pid.pp t.pid pp_state (Atomic.get t.state)
    (Mailbox.size t.save_queue + Mailbox.size t.mailbox)
    pp_flags t.flags

and pp_state ppf (state : state) =
  match state with
  | Runnable -> Format.fprintf ppf "Runnable"
  | Waiting_message -> Format.fprintf ppf "Waiting_message"
  | Waiting_io { syscall; token; _ } ->
      Format.fprintf ppf "Waiting_io(%s, %a)" syscall Gluon.Token.pp token
  | Running -> Format.fprintf ppf "Running"
  | Exited e -> Format.fprintf ppf "Exited(%a)" pp_reason e
  | Finalized -> Format.fprintf ppf "Finalized"
  | Uninitialized -> Format.fprintf ppf "Uninitialized"

and pp_reason ppf (t : exit_reason) =
  match t with
  | Normal -> Format.fprintf ppf "Normal"
  | Link_down pid -> Format.fprintf ppf "Link_down(%a)" Pid.pp pid
  | Exit_signal -> Format.fprintf ppf "Exit_signal"
  | Bad_link -> Format.fprintf ppf "Bad_link"
  | Exception exn -> Format.fprintf ppf "Exception: %s" (Printexc.to_string exn)

and pp_flags ppf (t : process_flags) =
  Format.fprintf ppf "{ trap_exits=%b; priority=%s }" (Atomic.get t.trap_exits)
    (priority_to_string @@ Atomic.get t.priority)

let cont t = t.cont |> Option.get
let pid { pid; _ } = pid
let sid { sid; _ } = sid
let state t = Atomic.get t.state
let monitors t = Pid.Map.keys t.monitors
let links t = Atomic.get t.links
let receive_timeout t = Atomic.get t.recv_timeout
let syscall_timeout t = Atomic.get t.syscall_timeout

let is_alive t =
  match Atomic.get t.state with
  | Finalized | Exited _ -> false
  | Uninitialized | Runnable | Waiting_message | Waiting_io _ | Running -> true

let is_exited t =
  match Atomic.get t.state with Finalized | Exited _ -> true | _ -> false

let is_waiting t =
  match Atomic.get t.state with
  | Waiting_io _ | Waiting_message -> true
  | _ -> false

let is_waiting_io t =
  match Atomic.get t.state with Waiting_io _ -> true | _ -> false

let is_runnable t = Atomic.get t.state = Runnable
let is_running t = Atomic.get t.state = Running
let is_finalized t = Atomic.get t.state = Finalized
let is_main t = Pid.equal (pid t) Pid.main

let has_empty_mailbox t =
  Mailbox.is_empty t.save_queue && Mailbox.is_empty t.mailbox

let add_ready_token t token source =
  Util.Lf_queue.push t.ready_tokens (token, source)

let get_ready_token t = Util.Lf_queue.pop t.ready_tokens

let rec consume_ready_tokens t fn =
  match get_ready_token t with
  | Some x ->
      fn x;
      consume_ready_tokens t fn
  | None -> ()

let has_messages t = not (has_empty_mailbox t)
let message_count t = Mailbox.size t.mailbox + Mailbox.size t.save_queue
let should_awake t = is_alive t && has_messages t

let rec set_syscall_timeout t timeout =
  let last_timeout = Atomic.get t.syscall_timeout in
  if Option.is_some last_timeout then failwith "overriding timeout";
  if Atomic.compare_and_set t.syscall_timeout last_timeout (Some timeout) then
    ()
  else set_syscall_timeout t timeout

let rec clear_syscall_timeout t =
  let last_timeout = Atomic.get t.syscall_timeout in
  if Option.is_none last_timeout then failwith "clearing empty timeout";
  if Atomic.compare_and_set t.syscall_timeout last_timeout None then ()
  else clear_syscall_timeout t

let rec set_receive_timeout t timeout =
  let last_timeout = Atomic.get t.recv_timeout in
  if Atomic.compare_and_set t.recv_timeout last_timeout (Some timeout) then ()
  else set_receive_timeout t timeout

let rec clear_receive_timeout t =
  let last_timeout = Atomic.get t.recv_timeout in
  if Atomic.compare_and_set t.recv_timeout last_timeout None then ()
  else clear_receive_timeout t

exception Process_reviving_is_forbidden of t

let rec mark_as_awaiting_io t syscall token source =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if
    Atomic.compare_and_set t.state old_state
      (Waiting_io { syscall; token; source })
  then
    Log.trace (fun f -> f "Process %a: marked as waiting for io" Pid.pp t.pid)
  else mark_as_awaiting_io t syscall token source

let rec mark_as_awaiting_message t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Waiting_message then
    Log.trace (fun f ->
        f "Process %a: marked as waiting for message" Pid.pp t.pid)
  else mark_as_awaiting_message t

let rec mark_as_running t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Running then
    Log.trace (fun f -> f "Process %a: marked as running" Pid.pp t.pid)
  else mark_as_running t

let rec mark_as_runnable t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Runnable then
    Log.trace (fun f -> f "Process %a: marked as runnable" Pid.pp t.pid)
  else mark_as_runnable t

let rec mark_as_exited t reason =
  if is_exited t then ()
  else
    let old_state = Atomic.get t.state in
    if Atomic.compare_and_set t.state old_state (Exited reason) then
      Log.trace (fun f ->
          f "Process %a: marked as exited with reason %a" Pid.pp t.pid pp_reason
            reason)
    else mark_as_exited t reason

let rec mark_as_finalized t =
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Finalized then
    Log.trace (fun f -> f "Process %a: marked as finalized" Pid.pp t.pid)
  else mark_as_finalized t

(** `set_flag` is only called by `Riot.process_flag` which runs only on the
    current process, which means we already have a lock on it.
 *)
let rec set_flag t flag =
  match flag with
  | Trap_exit v ->
      let old_flag = Atomic.get t.flags.trap_exits in
      if Atomic.compare_and_set t.flags.trap_exits old_flag v then ()
      else set_flag t flag
  | Priority p ->
      let old_flag = Atomic.get t.flags.priority in
      if Atomic.compare_and_set t.flags.priority old_flag p then ()
      else set_flag t flag

let set_cont t c = t.cont <- Some c

let rec add_link t link =
  let old_links = Atomic.get t.links in
  let new_links = link :: old_links in
  if Atomic.compare_and_set t.links old_links new_links then (
    Log.trace (fun f ->
        f "Process %a: adding link to %a" Pid.pp t.pid Pid.pp link);
    ())
  else add_link t link

let add_monitored_by t monitor =
  Log.trace (fun f ->
      f "Process %a: is being monitored by %a" Pid.pp t.pid Pid.pp monitor);
  Pid.Map.insert t.monitored_by monitor ()

let remove_monitored_by t monitor =
  Log.trace (fun f ->
      f "Process %a: is being monitored by %a" Pid.pp t.pid Pid.pp monitor);
  Pid.Map.remove t.monitored_by monitor

let add_monitor t monitor =
  Log.trace (fun f ->
      f "Process %a: adding monitor to %a" Pid.pp t.pid Pid.pp monitor);
  Pid.Map.insert t.monitors monitor ()

let remove_monitor t monitor =
  Log.trace (fun f ->
      f "Process %a: adding monitor to %a" Pid.pp t.pid Pid.pp monitor);
  Pid.Map.remove t.monitors monitor

let is_monitoring_pid t pid = Pid.Map.has_key t.monitors pid
let is_monitored_by_pid t pid = Pid.Map.has_key t.monitored_by pid

let next_message t =
  if t.read_save_queue then (
    match Mailbox.next t.save_queue with
    | Some m ->
        Log.trace (fun f ->
            f "Process %a: found message in save queue" Pid.pp t.pid);
        Some m
    | None ->
        t.read_save_queue <- false;
        None)
  else
    match Mailbox.next t.mailbox with
    | Some m ->
        Log.trace (fun f ->
            f "Process %a: found message in mailbox" Pid.pp t.pid);
        Some m
    | None -> None

let add_to_save_queue t msg = Mailbox.queue t.save_queue msg
let read_save_queue t = t.read_save_queue <- true

let send_message t msg =
  if is_alive t then (
    let envelope = Message.envelope msg in
    Mailbox.queue t.mailbox envelope;
    if is_waiting t then mark_as_runnable t)
