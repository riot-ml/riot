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
    | Profile :
        profile_req
        -> (user, [ `Twitch_error of error ]) result Gen_server.req

  type args = { verbose : bool }

  module Server : Gen_server.Impl with type args = args = struct
    type nonrec args = args
    type state = { status : int }

    let init _args = Gen_server.Ok { status = 1 }

    let handle_call : type res. res Gen_server.req -> Pid.t -> state -> res =
     fun req _from _state ->
      match req with
      | Is_connected -> true
      | Profile _ ->
          Ok { name = "Jonathan Archer"; email = "archer4eva@starfl.it" }

    let handle_info _msg _state = ()

    let handle_cast : type res. res Gen_server.req -> state -> unit =
     fun req _state -> match req with Profile _ -> ()
  end

  let start_link ?(verbose = false) () =
    Gen_server.start_link (module Server) { verbose }

  let is_connected pid = Gen_server.call pid Is_connected
  let profile pid ~id = Gen_server.call pid (Profile { id })
end

let main () =
  let (Ok _) = Logger.start () in
  let (Ok pid) = Twitch.start_link () in
  if Twitch.is_connected pid then Logger.info (fun f -> f "connected to twitch");
  let (Ok user) = Twitch.profile pid ~id:1 in
  Logger.info (fun f -> f "Welcome, %s!" user.name)

let () = Riot.run @@ main
