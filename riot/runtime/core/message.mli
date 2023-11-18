type t = ..
type select_marker = Take | Skip | Drop
type envelope = { msg : t; uid : unit Ref.t }

val envelope : t -> envelope
