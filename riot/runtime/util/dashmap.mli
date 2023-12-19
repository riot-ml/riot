type ('k, 'v) t

val create : 'a -> ('b, 'c) t
val is_empty : ('k, 'v) t -> bool
val entries : ('a, 'b) t -> ('a * 'b) list
val find_by : ('a, 'b) t -> ('a * 'b -> bool) -> ('a * 'b) option
val find_all_by : ('a, 'b) t -> ('a * 'b -> bool) -> ('a * 'b) list
val has_key : ('a, 'b) t -> 'a -> bool
val insert : ('a, 'b) t -> 'a -> 'b -> unit
val remove_by : ('a, 'b) t -> ('a * 'b -> bool) -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val iter : ('a, 'b) t -> ('a * 'b -> unit) -> unit

val pp :
  (Format.formatter -> 'k -> unit) -> Format.formatter -> ('k, 'v) t -> unit
