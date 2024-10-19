open Global

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "task" ]
end)

type 'a t = { pid : Pid.t; ref : 'a Ref.t }
type Message.t += Reply : 'a Ref.t * 'a -> Message.t

let async fn =
  let ref = Ref.make () in
  let this = self () in
  let pid =
    spawn (fun () ->
        Logger.trace (fun f -> f "spawned task %a" Pid.pp (self ()));
        let value = fn () in
        let reply = Reply (ref, value) in
        Logger.trace (fun f -> f "sending message back: %a" Pid.pp (self ()));
        send this reply)
  in
  Process.monitor pid;
  { pid; ref }

let await :
    type res.
    ?timeout:int64 -> res t -> (res, [> `Timeout | `Process_down ]) result =
 fun ?timeout:after t ->
  Logger.trace (fun f ->
      f "Process %a is awaing for task %a with timeout %Ld" Pid.pp (self ())
        Pid.pp t.pid
        (Option.value ~default:(-1L) after));
  let selector : [ `reply of res | `process_down ] Message.selector =
   fun msg ->
    match msg with
    | Reply (ref', res) when Ref.equal t.ref ref' -> (
        match Ref.type_equal t.ref ref' with
        | Some Type.Equal -> `select (`reply res)
        | None -> failwith "bad message")
    | Process.Messages.Monitor (Process_down pid) when Pid.equal pid t.pid ->
        `select `process_down
    | _ -> `skip
  in
  match receive ~selector ?after () with
  | exception Receive_timeout ->
      Logger.trace (fun f -> f "task %a timeout" Pid.pp t.pid);
      Error `Timeout
  | `reply res ->
      Process.demonitor t.pid;
      Ok res
  | `process_down -> Error `Process_down
