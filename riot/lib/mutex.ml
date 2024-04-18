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

(* NOTE: (@faycarsons) This should(?) prevent deadlocks caused by mutex
    process dyng before sending `LockAccepted` message *)
let wait_lock mutex =
  monitor mutex.process;
  send mutex.process @@ Lock (self ());
  match receive ~selector () with
  | Monitor (Process_down _) -> Error `process_died
  | Failed reason -> Error reason
  | _ -> Ok ()

let try_wait_lock mutex =
  monitor mutex.process;
  send mutex.process @@ TryLock (self ());
  match[@warning "-8"] receive ~selector () with
  | LockAccepted -> Ok ()
  | Failed reason -> Error reason
  | Monitor (Process_down _) -> Error `process_died

let wait_unlock mutex =
  send mutex.process @@ Unlock (self ());
  match
    receive
      ~selector:(function
        | (UnlockAccepted | Failed _) as msg -> `select msg | _ -> `skip)
      ()
  with
  | Failed reason -> Error reason
  | _ -> Ok ()

(* NOTE: (@faycarsons) Maybe use marshaling here instead? Unsure how much of a
    priority getting a true deep copy, I.E. for nested data structures, is vs
    cost of serialization. *)
let clone (x : 'a) : 'a = Obj.(repr x |> dup |> magic)

(* Exposed API *)

let lock t fn =
  let* _ = wait_lock t in
  fn t.inner;
  wait_unlock t

let try_lock mutex fn =
  let* _ = try_wait_lock mutex in
  fn mutex.inner;
  wait_unlock mutex

let get (t : 'a t) : ('a, err) result =
  let* _ = wait_lock t in
  let { inner; _ } = t in
  let* _ = wait_unlock t in
  Result.ok @@ clone inner

let try_get mutex =
  let* _ = try_wait_lock mutex in
  let { inner; _ } = mutex in
  let* _ = wait_unlock mutex in
  Result.ok @@ clone inner

(* NOTE: (@faycarsons) not sure if we want this? *)
let unsafe_get t = t.inner
