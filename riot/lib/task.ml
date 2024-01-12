open Global

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "task" ]
end)

type 'a t = { pid : Pid.t; ref : 'a Symbol.t }
type Message.t += Reply : 'a Symbol.t * 'a -> Message.t

let async fn =
  let ref = Symbol.make () in
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

let rec await :
    type res.
    ?timeout:int64 -> res t -> (res, [> `Timeout | `Process_down ]) result =
 fun ?timeout:after t ->
  let started_at =
    Int64.add (Mtime_clock.now_ns ()) (Option.value ~default:0L after)
  in
  Logger.trace (fun f ->
      f "Process %a is awaing for task %a with timeout %Ld" Pid.pp (self ())
        Pid.pp t.pid
        (Option.value ~default:(-1L) after));
  match receive ?after () with
  | exception Receive_timeout ->
      Logger.trace (fun f -> f "task %a timeout" Pid.pp t.pid);
      Error `Timeout
  | Reply (ref', res) when Symbol.equal t.ref ref' -> (
      Process.demonitor t.pid;
      match Symbol.type_equal t.ref ref' with
      | Some Type.Equal -> Ok res
      | None -> failwith "bad message")
  | Process.Messages.Monitor (Process_down pid) when Pid.equal pid t.pid ->
      Error `Process_down
  | Process.Messages.Monitor (Process_down pid) ->
      Logger.error (fun f ->
          f
            "received wrong monitor process down for %a (we are %a, our task \
             is %a)"
            Pid.pp pid Pid.pp (self ()) Pid.pp t.pid);
      Error `Process_down
  | msg ->
      Logger.trace (fun f ->
          f "requeuing message %S" (Marshal.to_string msg []));
      send (self ()) msg;
      let timeout = Int64.sub started_at (Mtime_clock.now_ns ()) in
      await ~timeout t
