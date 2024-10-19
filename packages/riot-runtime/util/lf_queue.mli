exception Closed

type 'a t

val push : 'a t -> 'a -> unit
val push_head : 'a t -> 'a -> unit
val close : 'a t -> unit
val peek : 'a t -> 'a
val pop : 'a t -> 'a option
val is_empty : 'a t -> bool
val create : unit -> 'a t
