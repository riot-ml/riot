open Runtime

type 'a t = { pid : Pid.t; ref : 'a Ref.t; value : 'a option }
type Message.t += Reply : 'a Ref.t * 'a -> Message.t

let async fn =
  let ref = Ref.make () in
  let this = self () in
  let pid =
    spawn (fun () ->
        Log.trace (fun f -> f "spawned task %a" Pid.pp (self ()));
        send this (Reply (ref, fn ())))
  in
  monitor this pid;
  { pid; ref; value = None }

let await :
    type res.
    ?timeout:int64 -> res t -> (res, [> `Timeout | `Process_down ]) result =
 fun ?timeout:after t ->
  match receive ?after ~ref:t.ref () with
  | exception Receive_timeout -> Error `Timeout
  | Process.Messages.Monitor (Process_down pid) when pid = t.pid ->
      Error `Process_down
  | Reply (ref', res) -> (
      match Ref.type_equal t.ref ref' with
      | Some Type.Equal -> Ok res
      | None -> failwith "bad message")
  | _ -> failwith "unexpected message"
