open Runtime

type 'res req = ..
type 'state init_result = Ok of 'state | Error | Ignore

module type Impl = sig
  type args
  type state

  val init : args -> state init_result
  val handle_call : 'res req -> Pid.t -> state -> 'res
  val handle_info : Message.t -> state -> unit
  val handle_cast : 'res req -> state -> unit
end

type ('args, 'state) impl =
  (module Impl with type args = 'args and type state = 'state)

val call : Pid.t -> 'res req -> 'res
val cast : Pid.t -> 'res req -> unit
val start_link : ('args, 'state) impl -> 'args -> (Pid.t, exn) result
