(** Mutable sequence of elements. *)
(* (c) Lwt authors *)

type 'a t
(** Type of a sequence holding values of type ['a]. *)

type 'a node
(** Type of a node holding one value of type ['a] in a sequence. *)

exception Empty
(** Exception raised by {!val:take} when the sequence is empty. *)

val create : Random.State.t -> 'a t
(** [create g] creates a new empty sequence. *)

val push : 'a -> 'a t -> unit
(** [push x s] adds [x] into the sequence [s]. *)

val take : 'a t -> 'a
(** [take s] remove and returns one element of [s].

    @raise Empty if the sequence is empty. *)

val take_r : 'a t -> 'a
val take_l : 'a t -> 'a
val add_r : 'a -> 'a t -> unit
val add_l : 'a -> 'a t -> unit

val drop : 'a t -> unit
(** Removes all nodes from the given sequence. The nodes are not actually
    mutated to not their removal. Only the sequence's pointers are update. *)

val length : 'a t -> int
(** Returns the number of elements in the given sequence. This is a [O(n)]
    operation where [n] is the number of elements in the sequence. *)

val iter : f:('a -> unit) -> 'a t -> unit
(** [iter ~f s] applies [f] on all elements of [s] starting from left. *)

val iter_node : f:('a node -> unit) -> 'a t -> unit
(** [iter_node ~f s] applies [f] on all nodes of [s] starting from left. *)

val is_empty : 'a t -> bool
(** Returns [true] iff the given sequence is empty. *)

val remove : 'a node -> unit
(** Removes a node from the sequence it is part of. It does nothing if the node
    has already been removed. *)

val data : 'a node -> 'a
(** Returns the contents of a node. *)

val to_list : 'a t -> 'a list
(** Returns the given sequence as a list. *)
