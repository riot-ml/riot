type 'a t

val drop : 'a t -> unit
val set : 'a t -> prev:'a -> next:'a -> bool
val make : 'a -> release:('a t -> 'a) -> 'a t
val get : 'a t -> 'a
val peek : 'a t -> 'a
val refc : 'a t -> int
val take : 'a t -> unit
val use : 'a t -> ('a -> 'b) -> 'b
