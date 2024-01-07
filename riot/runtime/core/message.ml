type t = ..
type select_marker = Take | Skip | Drop
type envelope = { msg : t; uid : unit Symbol.t }

let envelope msg = { uid = Symbol.make (); msg }
