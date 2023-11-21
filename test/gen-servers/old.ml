[@@@warning "-8"]
[@@@warning "-37"]
[@@@warning "-69"]
[@@@warning "-38"]
[@@@warning "-32"]

open Riot
module Gadt = Gadt

type reason = Normal | Shutdown
type timeout = int

type 'continue continue =
  | Yield
  | Timeout of timeout
  | Hibernate
  | Continue of 'continue

type ('res, 'state, 'continue) call_reply =
  | Reply of { reply : 'res; state : 'state; mode : 'continue continue }
  | No_reply of { state : 'state; mode : 'continue continue }
  | Stop of { reason : reason; reply : 'res option; state : 'state }

type ('state, 'continue) cast_reply =
  | No_reply of { state : 'state; mode : 'continue continue }
  | Stop of { reason : reason; state : 'state }

type ('state, 'continue) init_result =
  | Init_ok of 'state * 'continue continue
  | Init_error
  | Ignore

module type Base_server = sig
  type args
  type state
  type response
  type continue
  type call_req
  type cast_req

  val init : args -> (state, continue) init_result
  val terminate : reason -> state -> unit

  val handle_call :
    call_req -> Pid.t -> state -> (response, state, continue) call_reply

  val handle_cast : cast_req -> state -> (state, continue) cast_reply
end

module Gen_server (B : Base_server) = struct
  type pid = Pid of Pid.t

  type Message.t +=
    | Call of Uid.t * B.call_req
    | Cast of Uid.t * B.cast_req
    | Res of Uid.t * B.response

  let rec loop state =
    yield ();
    loop state

  let init args =
    match B.init args with
    | Init_ok (state, _continue) -> loop state
    | Init_error | Ignore -> ()

  let start_link args =
    let pid = spawn_link (fun () -> init args) in
    Ok (Pid pid)

  let call (Pid pid) req =
    let uref = Uid.next () in
    send pid (Call (uref, req));
    let select = function
      | Res (uref', _res) when Uid.equal uref uref' -> Message.Take
      | _ -> Message.Skip
    in
    match receive ~select () with
    | Res (_, res) -> res
    | _ -> failwith "wrong message!"

  let cast (Pid pid) req = send pid (Cast (Uid.next (), req))
end

module Twitch : sig
  type pid
  type args = { verbose : bool }
  type response = Response

  val start_link : args -> (pid, exn) result
  val is_connected : pid -> response
end = struct
  type args = { verbose : bool }
  type response = Response

  module Base = struct
    type nonrec args = args
    type nonrec response = response
    type state = { calls : int }
    type continue = unit
    type call_req = Is_connected
    type cast_req = Do_cast

    let init _args = Init_ok ({ calls = 0 }, Yield)
    let terminate _reason _state = ()

    let handle_call _req _from state =
      Reply { reply = Response; state; mode = Yield }

    let handle_cast _req state = No_reply { state; mode = Yield }
  end

  include Gen_server (Base)

  let is_connected pid = call pid Is_connected
end

let main () =
  let (Ok ()) = Logger.start () in
  let (Ok pid) = Twitch.(start_link { verbose = true }) in
  let _ = Twitch.is_connected pid in
  Logger.info (fun f -> f "started")

let () = Riot.run @@ main
