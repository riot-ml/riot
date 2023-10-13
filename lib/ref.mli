type 'a t

val make : unit -> 'a t
val equal : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option
val pp : Format.formatter -> 'a t -> unit
val is_newer : 'a t -> 'b t -> bool
