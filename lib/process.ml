type exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

module Messages = struct
  type monitor = Process_down of Pid.t
  type Message.t += Monitor of monitor | Exit of Pid.t * exit_reason
end

type state = Runnable | Waiting | Running | Exited of exit_reason | Finalized
type process_flags = { trap_exits : bool Atomic.t }
type process_flag = Trap_exit of bool

let default_flags () = { trap_exits = Atomic.make false }

type t = {
  pid : Pid.t;
  sid : Scheduler_uid.t;
  flags : process_flags;
  state : state Atomic.t;
  mutable cont : exit_reason Proc_state.t;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable read_save_queue : bool;
      (** the save queue is a temporary queue used for storing messages during a selective receive *)
  links : Pid.t list Atomic.t;
  monitors : Pid.t list Atomic.t;
}
(** The process descriptor. *)

let make sid fn =
  let cont = Proc_state.make fn Proc_effect.Yield in
  let pid = Pid.next () in
  Logs.debug (fun f -> f "Making process with pid: %a" Pid.pp pid);
  let proc =
    {
      pid;
      sid;
      cont;
      state = Atomic.make Runnable;
      links = Atomic.make [];
      monitors = Atomic.make [];
      mailbox = Mailbox.create ();
      save_queue = Mailbox.create ();
      read_save_queue = false;
      flags = default_flags ();
    }
  in
  proc

let rec pp ppf t =
  Format.fprintf ppf "Process%a { state = %a; messages = %d; flags = %a }"
    Pid.pp t.pid pp_state (Atomic.get t.state)
    (Mailbox.size t.save_queue + Mailbox.size t.mailbox)
    pp_flags t.flags

and pp_state ppf (state : state) =
  match state with
  | Runnable -> Format.fprintf ppf "Runnable"
  | Waiting -> Format.fprintf ppf "Waiting"
  | Running -> Format.fprintf ppf "Running"
  | Exited e -> Format.fprintf ppf "Exited(%a)" pp_reason e
  | Finalized -> Format.fprintf ppf "Finalized"

and pp_reason ppf (t : exit_reason) =
  match t with
  | Normal -> Format.fprintf ppf "Normal"
  | Link_down pid -> Format.fprintf ppf "Link_down(%a)" Pid.pp pid
  | Exit_signal -> Format.fprintf ppf "Exit_signal"
  | Bad_link -> Format.fprintf ppf "Bad_link"
  | Exception exn -> Format.fprintf ppf "Exception: %s" (Printexc.to_string exn)

and pp_flags ppf (t : process_flags) =
  Format.fprintf ppf "{ trap_exits=%b }" (Atomic.get t.trap_exits)

let cont t = t.cont
let get_pid { pid; _ } = pid
let pid { pid; _ } = pid
let sid { sid; _ } = sid
let state t = Atomic.get t.state
let monitors t = Atomic.get t.monitors
let links t = Atomic.get t.links

let is_alive t =
  match Atomic.get t.state with
  | Runnable | Waiting | Running -> true
  | Finalized | Exited _ -> false

let is_exited t =
  match Atomic.get t.state with Finalized | Exited _ -> true | _ -> false

let is_runnable t = Atomic.get t.state = Runnable
let is_running t = Atomic.get t.state = Running
let is_waiting t = Atomic.get t.state = Waiting
let is_finalized t = Atomic.get t.state = Finalized

let has_empty_mailbox t =
  Mailbox.is_empty t.save_queue && Mailbox.is_empty t.mailbox

let has_messages t = not (has_empty_mailbox t)
let message_count t = Mailbox.size t.mailbox + Mailbox.size t.save_queue
let should_awake t = is_alive t && has_messages t

exception Process_reviving_is_forbidden of t

let rec mark_as_awaiting_message t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Waiting then
    Logs.trace (fun f -> f "Process %a: marked as waiting" Pid.pp t.pid)
  else mark_as_awaiting_message t

let rec mark_as_running t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Running then
    Logs.trace (fun f -> f "Process %a: marked as running" Pid.pp t.pid)
  else mark_as_running t

let rec mark_as_runnable t =
  if is_exited t then raise (Process_reviving_is_forbidden t);
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Runnable then
    Logs.trace (fun f -> f "Process %a: marked as runnable" Pid.pp t.pid)
  else mark_as_runnable t

let rec mark_as_exited t reason =
  if is_exited t then ()
  else
    let old_state = Atomic.get t.state in
    if Atomic.compare_and_set t.state old_state (Exited reason) then
      Logs.trace (fun f ->
          f "Process %a: marked as exited with reason %a" Pid.pp t.pid pp_reason
            reason)
    else mark_as_exited t reason

let rec mark_as_finalized t =
  let old_state = Atomic.get t.state in
  if Atomic.compare_and_set t.state old_state Finalized then
    Logs.trace (fun f -> f "Process %a: marked as finalized" Pid.pp t.pid)
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

let set_cont t c = t.cont <- c

let rec add_link t link =
  let old_links = Atomic.get t.links in
  let new_links = link :: old_links in
  if Atomic.compare_and_set t.links old_links new_links then (
    Logs.trace (fun f ->
        f "Process %a: adding link to %a" Pid.pp t.pid Pid.pp link);
    ())
  else add_link t link

let rec add_monitor t monitor =
  let old_monitors = Atomic.get t.monitors in
  let new_monitors = monitor :: old_monitors in
  if Atomic.compare_and_set t.monitors old_monitors new_monitors then (
    Logs.trace (fun f ->
        f "Process %a: adding monitor to %a" Pid.pp t.pid Pid.pp monitor);
    ())
  else add_monitor t monitor

let next_message t =
  if t.read_save_queue then (
    match Mailbox.next t.save_queue with
    | Some m ->
        Logs.trace (fun f ->
            f "Process %a: found message in save queue" Pid.pp t.pid);
        Some m
    | None ->
        t.read_save_queue <- false;
        None)
  else
    match Mailbox.next t.mailbox with
    | Some m ->
        Logs.trace (fun f ->
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
