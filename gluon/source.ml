open Io
open Gluon_common
open Gluon_sys

module type Intf = sig
  type t

  val deregister : t -> Sys.Selector.t -> (unit, [> `Noop ]) io_result

  val register :
    t -> Sys.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result

  val reregister :
    t -> Sys.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result
end

type t = S : ((module Intf with type t = 'state) * 'state) -> t

let make src state = S (src, state)
let register (S ((module Src), state)) = Src.register state
let reregister (S ((module Src), state)) = Src.reregister state
let deregister (S ((module Src), state)) = Src.deregister state
