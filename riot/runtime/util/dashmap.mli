type ('k, 'v) t

val create : int -> ('k, 'v) t
val get : ('k, 'v) t -> 'k -> 'v list
val is_empty : ('k, 'v) t -> bool
val find_by : ('k, 'v) t -> ('k * 'v -> bool) -> ('k * 'v) option
val remove : ('k, 'v) t -> 'k -> unit
val remove_all : ('k, 'v) t -> 'k list -> unit
val find_all_by : ('k, 'v) t -> ('k * 'v -> bool) -> ('k * 'v) list
val has_key : ('k, 'v) t -> 'k -> bool
val insert : ('k, 'v) t -> 'k -> 'v -> unit
val remove_by : ('k, 'v) t -> ('k * 'v -> bool) -> unit
val replace : ('k, 'v) t -> 'k -> 'v -> unit
val iter : ('k, 'v) t -> ('k * 'v -> unit) -> unit

val pp :
  (Format.formatter -> 'k -> unit) -> Format.formatter -> ('k, 'v) t -> unit
