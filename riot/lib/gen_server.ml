open Runtime

type 'res req = ..

type Message.t +=
  | Call : Pid.t * 'res Ref.t * 'res req -> Message.t
  | Cast : 'res req -> Message.t
  | Reply : 'res Ref.t * 'res -> Message.t

type 'state init_result = Ok of 'state | Error | Ignore

module type Impl = sig
  type args
  type state

  val init : args -> state init_result
  val handle_call : 'res. 'res req -> Pid.t -> state -> 'res
  val handle_info : Message.t -> state -> unit
  val handle_cast : 'res. 'res req -> state -> unit
end

type ('args, 'state) impl =
  (module Impl with type args = 'args and type state = 'state)

let call : type res. Pid.t -> res req -> res =
 fun pid req ->
  let ref = Ref.make () in
  send pid (Call (self (), ref, req));
  match receive () with
  | Reply (ref', res) -> (
      match Ref.type_equal ref ref' with
      | Some Type.Equal -> res
      | None -> failwith "bad message")
  | _ -> failwith "unexpected message"

let cast : type res. Pid.t -> res req -> unit =
 fun pid req ->
  send pid (Cast req);
  ()

let rec loop : type args state. (args, state) impl -> state -> unit =
 fun impl state ->
  let (module I : Impl with type args = args and type state = state) = impl in
  match receive () with
  | Call (pid, ref, req) ->
      let res = I.handle_call req pid state in
      send pid (Reply (ref, res));
      loop impl state
  | Cast req ->
      I.handle_cast req state;
      loop impl state
  | msg ->
      let _res = I.handle_info msg state in
      loop impl state

let start_link :
    type args state. (args, state) impl -> args -> (Pid.t, exn) result =
 fun impl args ->
  let pid =
    spawn_link (fun () ->
        let (module I : Impl with type args = args and type state = state) =
          impl
        in
        match I.init args with
        | Ok state -> loop impl state
        | Error | Ignore -> ())
  in
  Ok pid
