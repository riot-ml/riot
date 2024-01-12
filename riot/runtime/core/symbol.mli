type 'a t

val make : unit -> 'a t
val equal : 'a t -> 'b t -> bool
val hash : 'a t -> int
val type_equal : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option
val pp : Format.formatter -> 'a t -> unit
val is_newer : 'a t -> 'b t -> bool

module Map : Util.Dashmap.Intf with type key = unit t
