module Exn : sig
  exception Name_already_registered of string * Pid.t
end

type t

val create : unit -> t
val register : t -> string -> Pid.t -> unit
val unregister : t -> string -> unit
val find_pid : t -> string -> Pid.t option
