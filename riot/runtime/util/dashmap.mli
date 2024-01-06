type ('k, 'v) t

val create : int -> ('k, 'v) t
val get : ('k, 'v) t -> 'k -> 'v option
val get_all : ('k, 'v) t -> 'k -> 'v list
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

module type Base = sig
  type key

  val hash : key -> int
  val equal : key -> key -> bool
end

module type Intf = sig
  type key
  type 'v t

  val create : int -> 'v t
  val keys : 'v t -> key Seq.t
  val get : 'v t -> key -> 'v option
  val get_all : 'v t -> key -> 'v list
  val is_empty : 'v t -> bool
  val find_by : 'v t -> (key * 'v -> bool) -> (key * 'v) option
  val remove : 'v t -> key -> unit
  val remove_all : 'v t -> key list -> unit
  val find_all_by : 'v t -> (key * 'v -> bool) -> (key * 'v) list
  val has_key : 'v t -> key -> bool
  val insert : 'v t -> key -> 'v -> unit
  val remove_by : 'v t -> (key * 'v -> bool) -> unit
  val replace : 'v t -> key -> 'v -> unit
  val iter : 'v t -> (key * 'v -> unit) -> unit
  val pp : (Format.formatter -> key -> unit) -> Format.formatter -> 'v t -> unit
end

module Make (B : Base) : Intf with type key = B.key
