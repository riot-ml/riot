open Global

type 'res req = ..

type Message.t +=
  | Call : Pid.t * 'res Ref.t * 'res req -> Message.t
  | Reply : 'res Ref.t * 'res -> Message.t

type 'state init_result = Ok of 'state | Error | Ignore

module type Impl = sig
  type args
  type state

  val init : args -> state init_result
  val handle_call : 'res. 'res req -> Pid.t -> state -> 'res * state
  val handle_info : Message.t -> state -> unit
end

type ('args, 'state) impl =
  (module Impl with type args = 'args and type state = 'state)

let call : type res. Pid.t -> res req -> res =
 fun pid req ->
  let ref = Ref.make () in
  send pid (Call (self (), ref, req));
  let selector : res Message.selector =
   fun msg ->
    match msg with
    | Reply (ref', res) -> (
        match Ref.type_equal ref ref' with
        | Some Type.Equal -> `select res
        | None -> failwith "bad message")
    | _ -> `skip
  in
  receive ~selector ()

let rec loop : type args state. (args, state) impl -> state -> unit =
 fun impl state ->
  let (module I : Impl with type args = args and type state = state) = impl in
  match receive_any () with
  | Call (pid, ref, req) ->
      let res, state = I.handle_call req pid state in
      send pid (Reply (ref, res));
      loop impl state
  | msg ->
      let _res = I.handle_info msg state in
      loop impl state

let start_link :
    type args state.
    (args, state) impl -> args -> (Pid.t, [> `Exn of exn ]) result =
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

module Default = struct
  let init _args = Ignore
  let handle_call _req _from _state = failwith "unimplemented"
  let handle_info _msg _state = failwith "unimplemented"
end
