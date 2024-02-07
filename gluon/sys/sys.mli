open Rio
open Gluon_common
open Gluon_events

module Selector : sig
  type t

  val name : string
  val make : unit -> (t, [> `Noop ]) io_result

  val select :
    ?timeout:int64 ->
    ?max_events:int ->
    t ->
    (Event.t list, [> `Noop ]) io_result

  val register :
    t ->
    fd:Fd.t ->
    token:Token.t ->
    interest:Interest.t ->
    (unit, [> `Noop ]) io_result

  val reregister :
    t ->
    fd:Fd.t ->
    token:Token.t ->
    interest:Interest.t ->
    (unit, [> `Noop ]) io_result

  val deregister : t -> fd:Fd.t -> (unit, [> `Noop ]) io_result
end

module Event : sig
  type t
end
