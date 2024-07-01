module type Base = sig
  type key
  type value
end

module MakeServer (B : Base) = struct
  include Gen_server.Default

  type _ Gen_server.req +=
    | Get : B.key -> B.value option Gen_server.req
    | Put : B.key * B.value -> unit Gen_server.req

  type args = unit
  type state = { tbl : (B.key, B.value) Hashtbl.t }

  let init () = Gen_server.Ok { tbl = Hashtbl.create 0 }

  let handle_call :
      type res.
      res Gen_server.req ->
      Pid.t ->
      state ->
      (res, state) Gen_server.call_result =
   fun req _from state ->
    match req with
    | Get k -> Gen_server.Reply (Hashtbl.find_opt state.tbl k, state)
    | Put (k, v) -> Gen_server.Reply (Hashtbl.replace state.tbl k v, state)
    | _ -> failwith "invalid call"
end

module type Intf = sig
  type key
  type value

  val start_link : unit -> (Pid.t, [> `Exn of exn ]) result
  val get : Pid.t -> key -> value option
  val put : Pid.t -> key -> value -> unit
  val child_spec : Supervisor.child_spec
end

module Make (B : Base) = struct
  module Server = MakeServer (B)

  type key = B.key
  type value = B.value

  let start_link () = Gen_server.start_link (module Server) ()
  let get pid key = Gen_server.call pid Server.(Get key)
  let put pid key value = Gen_server.call pid Server.(Put (key, value))
  let child_spec = Supervisor.child_spec start_link ()
end
