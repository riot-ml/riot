[@@@warning "-8"]
[@@@warning "-37"]
[@@@warning "-69"]
[@@@warning "-32"]

open Riot

module Twitch = struct
  module Logger = Logger.Make (struct
    let namespace = [ "twitch" ]
  end)

  let info = Logger.info

  type user = { name : string; email : string }
  type profile_req = { id : int }
  type error = [ `Bad_user_id of int ]

  type _ Gen_server.req +=
    | Is_connected : bool Gen_server.req
    | Status_value : int Gen_server.req
    | Profile :
        profile_req
        -> (user, [ `Twitch_error of error ]) result Gen_server.req

  type Gen_server.cont_req += Update_status : int -> Gen_server.cont_req
  type args = { verbose : bool }

  module Server : Gen_server.Impl with type args = args = struct
    type nonrec args = args
    type state = { status : int }

    let init _args = Gen_server.Ok { status = 1 }

    let handle_call :
        type res.
        res Gen_server.req ->
        Pid.t ->
        state ->
        (res, state) Gen_server.call_result =
     fun req _from state ->
      match req with
      | Is_connected -> Gen_server.Reply (true, state)
      | Status_value -> Gen_server.Reply (state.status, state)
      | Profile _ ->
          Gen_server.Reply_continue
            ( Ok { name = "Jonathan Archer"; email = "archer4eva@starfl.it" },
              state,
              Update_status 2 )

    let handle_info _msg _state = ()

    let handle_continue cont_req _state =
      match cont_req with Update_status n -> { status = n }

    let handle_cast _cast_req _state = failwith "unimplemented"
  end

  let start_link ?(verbose = false) () =
    Gen_server.start_link (module Server) { verbose }

  let is_connected pid = Gen_server.call pid Is_connected
  let profile pid ~id = Gen_server.call pid (Profile { id })
  let status pid = Gen_server.call pid Status_value
end

let main () =
  let (Ok _) = Logger.start () in
  let (Ok pid) = Twitch.start_link () in
  if Twitch.is_connected pid then Logger.info (fun f -> f "connected to twitch");
  let status = Twitch.status pid in
  Logger.info (fun f -> f "Status is %d" status);
  let (Ok user) = Twitch.profile pid ~id:1 in
  Logger.info (fun f -> f "Welcome, %s!" user.name);
  let status = Twitch.status pid in
  Logger.info (fun f -> f "Status is %d" status)

let () = Riot.run @@ main
