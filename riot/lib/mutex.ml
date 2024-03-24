module type Base = sig
  type value
end

module MakeServer (B : Base) = struct
  open Global
  open Util

  type value = B.value
  type state = { value : value; queue : Pid.t Lf_queue.t }

  type Message.t +=
    | Lock of Pid.t
    | Unlock of (Pid.t * B.value)
    | LockAck of (Pid.t * value)

  let rec loop_locked state locker_pid =
    match receive () with
    | Lock pid ->
        let () = Lf_queue.push state.queue pid in
        loop_locked state locker_pid
    | Unlock (pid, value) when locker_pid = pid ->
        let () = demonitor locker_pid in
        loop_unlocked { state with value }
    | Unlock (_, _) -> failwith "wrong pid tried to unlock mutex"
    | Process.Messages.Monitor (Process_down fell_pid)
      when locker_pid = fell_pid ->
        Logger.debug (fun f -> f "locker process crashed");
        loop_unlocked state
    | _ -> failwith "unexpected message"

  and loop_unlocked state =
    match Lf_queue.pop state.queue with
    | Some pid ->
        let () = send pid (LockAck (self (), state.value)) in
        let () = monitor pid in
        loop_locked state pid
    | None -> (
        match receive () with
        | Lock pid ->
            let () = send pid (LockAck (self (), state.value)) in
            let () = monitor pid in
            loop_locked state pid
        | _ -> failwith "unexpected message")

  let start_link value =
    let state = { queue = Lf_queue.create (); value } in
    (fun () -> loop_unlocked state) |> spawn_link |> Result.ok
end

module type Intf = sig
  type value
  type t

  val start_link : value -> (t, [> `Exn of exn ]) result
  val lock : t -> value
  val unlock : t -> value -> unit
end

module Make (B : Base) = struct
  open Global
  module Server = MakeServer (B)

  type value = B.value
  type t = Pid.t

  let start_link = Server.start_link

  (* PR-Note (from: @julien-leclercq): How to handle the case of a dead mutex process ?*)
  let lock mutex =
    let () = send mutex @@ Server.Lock (self ()) in
    let rec do_receive () =
      match receive () with
      | Server.(LockAck (sender, value)) when sender = mutex -> value
      | msg ->
          send (self ()) msg;
          do_receive ()
    in
    do_receive ()

  let unlock mutex new_value = send mutex Server.(Unlock (self (), new_value))
end
