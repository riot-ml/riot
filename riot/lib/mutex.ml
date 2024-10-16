open Global
open Util
open Process.Messages

type 'a t = { mutable inner : 'a; process : Pid.t }

type state = { status : status; queue : Pid.t Lf_queue.t }
and status = Locked of Pid.t | Unlocked

let pp fmt inner_pp mutex =
  Format.fprintf fmt "Mutex<inner: %a>" inner_pp mutex.inner

type error = [ `multiple_unlocks | `locked | `not_owner | `process_died ]

let pp_err fmt error =
  let reason =
    match error with
    | `multiple_unlocks -> "Mutex received multiple unlock messages"
    | `locked -> "Mutex is locked"
    | `not_owner -> "Process does not own mutex"
    | `process_died -> "Mutex process died"
  in
  Format.fprintf fmt "Mutex error: %s" reason

type Message.t +=
  | Lock of Pid.t
  | Unlock of Pid.t
  | Try_lock of Pid.t
  | Lock_accepted
  | Unlock_accepted
  | Failed of error

let rec loop ({ status; queue } as state) =
  match receive_any () with
  | (Lock owner | Try_lock owner) when status = Unlocked ->
      monitor owner;
      send owner Lock_accepted;
      loop { state with status = Locked owner }
  | Lock requesting ->
      Lf_queue.push queue requesting;
      loop state
  | Try_lock requesting -> send requesting @@ Failed `locked
  | Unlock pid when status = Locked pid ->
      send pid Unlock_accepted;
      demonitor pid;
      check_queue { state with status = Unlocked }
  | Unlock not_owner when status = Unlocked ->
      Logger.error (fun f ->
          f "Mutex (PID: %a) received unlock message while unlocked" Pid.pp
            (self ()));
      send not_owner @@ Failed `multiple_unlocks;
      loop state
  | Unlock not_owner ->
      Logger.error (fun f ->
          f "Mutex (PID: %a) received unlock message from non-owner process"
            Pid.pp (self ()));
      send not_owner @@ Failed `not_owner;
      loop state
  | Monitor (Process_down fell_pid) when status = Locked fell_pid ->
      Logger.error (fun f -> f "Mutex owner crashed: %a" Pid.pp fell_pid);
      check_queue { state with status = Unlocked }
  | _ ->
      Logger.debug (fun f ->
          f "Mutex (PID: %a) received unexpected message" Pid.pp (self ()));
      loop state

and check_queue ({ queue; _ } as state) =
  match Lf_queue.pop queue with
  | Some owner ->
      send owner Lock_accepted;
      monitor owner;
      loop { state with status = Locked owner }
  | None -> loop state

let selector = function
  | (Lock_accepted | Unlock_accepted | Failed _ | Monitor (Process_down _)) as m
    ->
      `select m
  | _ -> `skip

(* Monitor mutex process to catch crashes *)
let wait_lock mutex : (unit, [> error ]) result =
  monitor mutex.process;
  send mutex.process @@ Lock (self ());
  match[@warning "-8"] receive ~selector () with
  | Monitor (Process_down _) -> Error `process_died
  | Failed reason -> Error reason
  | Lock_accepted -> Ok ()

let try_wait_lock mutex =
  monitor mutex.process;
  send mutex.process @@ Try_lock (self ());
  match[@warning "-8"] receive ~selector () with
  | Lock_accepted -> Ok ()
  | Failed reason -> Error reason
  | Monitor (Process_down _) -> Error `process_died

let wait_unlock mutex =
  send mutex.process @@ Unlock (self ());
  match[@warning "-8"] receive ~selector () with
  | Unlock_accepted ->
      demonitor mutex.process;
      Ok ()
  | Failed reason -> Error reason
  | Monitor (Process_down _) -> Error `process_died

(* NOTE: (@faycarsons) Assuming that we do want functions like `get` to return
   a copy of the wrapped value to prevent mutation once the mutex has been
   unlocked: I'm not sure how we want to go about that copying. There are maybe
   cheaper but less safe solutions using `Obj`, but if the serialization cost
   is OK this seems to work fine *)
let clone (inner : 'a) : 'a =
  let open Marshal in
  let ser = to_bytes inner [ Closures; No_sharing ] in
  from_bytes ser 0

(* Exposed API *)

let create inner =
  let state = { status = Unlocked; queue = Lf_queue.create () } in
  let process = spawn_link @@ fun () -> loop state in
  { inner; process }

let drop mutex = exit mutex.process Process.Normal

let lock mutex fn =
  let* _ = wait_lock mutex in
  mutex.inner <- fn mutex.inner;
  wait_unlock mutex

let try_lock mutex fn =
  let* _ = try_wait_lock mutex in
  mutex.inner <- fn mutex.inner;
  wait_unlock mutex

let iter mutex fn =
  let* _ = wait_lock mutex in
  fn mutex.inner;
  wait_unlock mutex

let try_iter mutex fn =
  let* _ = try_wait_lock mutex in
  fn mutex.inner;
  wait_unlock mutex

let get mutex =
  let* _ = wait_lock mutex in
  let inner = clone mutex.inner in
  let* _ = wait_unlock mutex in
  Ok inner

let try_get mutex =
  let* _ = try_wait_lock mutex in
  let inner = clone mutex.inner in
  let* _ = wait_unlock mutex in
  Ok inner

(* NOTE: (@faycarsons) not sure if we want this? *)
let unsafe_get mutex = mutex.inner
let unsafe_set mutex inner = mutex.inner <- inner
