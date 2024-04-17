open Global
open Util
open Process.Messages

type 'a t = { mutable inner : 'a; process : Pid.t }

type state = { status : status; queue : Pid.t Lf_queue.t }
and status = Locked of Pid.t | Unlocked

type err = [ `multiple_unlocks | `locked | `wrong_owner | `process_died ]

type Message.t +=
  | Lock of Pid.t
  | Unlock of Pid.t
  | TryLock of Pid.t
  | LockAccepted
  | UnlockAccepted
  | Failed of err

let rec loop ({ status; queue } as state) =
  match receive_any () with
  | Lock owner -> (
      match status with
      | Locked _ -> Lf_queue.push queue owner
      | Unlocked ->
          monitor owner;
          send owner LockAccepted;
          loop { state with status = Locked owner })
  | Unlock pid when status = Locked pid ->
      send pid UnlockAccepted;
      demonitor pid;
      check_queue { state with status = Unlocked }
  | Unlock _ -> (
      match status with
      | Locked wrong_owner ->
          Logger.error (fun f ->
              f "Mutex (PID: %a) received unlock from non-owning process" Pid.pp
              @@ self ());
          send wrong_owner @@ Failed `wrong_owner
      | Unlocked ->
          Logger.error (fun f ->
              f "Mutex (PID: %a) received unlock message while unlocked" Pid.pp
              @@ self ()))
  | TryLock owner -> (
      match status with
      | Locked _current -> send owner @@ Failed `locked
      | Unlocked ->
          monitor owner;
          send owner LockAccepted;
          loop { state with status = Locked owner })
  | Monitor (Process_down fell_pid) when status = Locked fell_pid ->
      Logger.error (fun f -> f "Mutex owner crashed: %a" Pid.pp fell_pid);
      loop { state with status = Unlocked }
  | _ -> loop state

and check_queue ({ queue; _ } as state) =
  match Lf_queue.pop queue with
  | Some pid ->
      send pid LockAccepted;
      monitor pid;
      loop { state with status = Locked pid }
  | None -> loop state

let create inner =
  let state = { status = Unlocked; queue = Lf_queue.create () } in
  let process = spawn_link @@ fun () -> loop state in
  { inner; process }

let selector = function
  | (LockAccepted | Failed _ | Monitor (Process_down _)) as m -> `select m
  | _ -> `skip

(*
    NOTE: (@faycarsons) The intention behind monitoring and listening for any 
    messages that could signal failure is to prevent deadlocks in the event 
    the mutex process somehow dies before sending a `LockAccepted` message.
*)
let wait_lock ({ process; _ } : 'a t) =
  monitor process;
  send process @@ Lock (self ());
  match receive ~selector () with
  | Monitor (Process_down _) -> Error `process_died
  | Failed reason -> Error reason
  | _ -> Ok ()

let try_wait_lock t =
  monitor t.process;
  send t.process @@ TryLock (self ());
  match[@warning "-8"] receive ~selector () with
  | LockAccepted -> Ok t.inner
  | Failed reason -> Error reason
  | Monitor (Process_down _) -> Error `process_died

let wait_unlock { process; _ } =
  send process @@ Unlock (self ());
  match
    receive
      ~selector:(function
        | (UnlockAccepted | Failed _) as msg -> `select msg | _ -> `skip)
      ()
  with
  | Failed reason -> Error reason
  | _ -> Ok ()

(* Exposed API *)

let lock t =
  let* _ = wait_lock t in
  Ok t.inner

let try_lock t =
  let* _ = try_wait_lock t in
  Ok t.inner

let unlock t = wait_unlock t

let with_lock t fn =
  let* _ = wait_lock t in
  fn t.inner;
  let* _ = wait_unlock t in
  Ok ()

let try_with_lock t fn =
  let* _ = try_wait_lock t in
  fn t.inner;
  let* _ = wait_unlock t in
  Ok ()

let with_inner t fn =
  let* _ = wait_lock t in
  let res = fn t.inner in
  let* _ = wait_unlock t in
  Ok res

(* NOTE(@faycarsons): not sure if we want these? *)
let unsafe_get t = t.inner
let unsafe_set t v = t.inner <- v
