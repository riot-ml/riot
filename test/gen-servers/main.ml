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
    | Put_status : int -> unit Gen_server.req
    | Status : int Gen_server.req
    | Profile :
        profile_req
        -> (user, [ `Twitch_error of error ]) result Gen_server.req

  type args = { verbose : bool }

  module Server : Gen_server.Impl with type args = args = struct
    type nonrec args = args
    type state = { status : int }

    let init _args = Gen_server.Ok { status = 1 }

    let handle_call :
        type res. res Gen_server.req -> Pid.t -> state -> res * state =
     fun req _from state ->
      match req with
      | Is_connected -> (true, state)
      | Status -> (state.status, state)
      | Profile _ ->
          ( Ok { name = "Jonathan Archer"; email = "archer4eva@starfl.it" },
            state )

    let handle_cast : type res. res Gen_server.req -> state -> state =
     fun req _ ->
      match req with
      | Put_status n -> { status = n }
      | _ -> failwith "invalid req"

    let handle_info _msg _state = ()
  end

  let start_link ?(verbose = false) () =
    Gen_server.start_link (module Server) { verbose }

  let is_connected pid = Gen_server.call pid Is_connected
  let profile pid ~id = Gen_server.call pid (Profile { id })
  let status pid = Gen_server.call pid Status
  let put_status pid n = Gen_server.cast pid (Put_status n)
end

let main () =
  let (Ok _) = Logger.start () in
  let (Ok pid) = Twitch.start_link () in
  if Twitch.is_connected pid then Logger.info (fun f -> f "connected to twitch");
  let (Ok user) = Twitch.profile pid ~id:1 in
  Logger.info (fun f -> f "Welcome, %s!" user.name);
  let () = Twitch.put_status pid 10 in
  let status = Twitch.status pid in
  Logger.info (fun f -> f "Status is %d" status)

let () = Riot.run @@ main
