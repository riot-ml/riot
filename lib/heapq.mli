(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Traditional implementation of priority queues using a binary heap
    encoded in a resizable array.

    When documenting complexity below, [n] refers to the number of elements
    in the queue.

    The size of the internal array is doubled when insertion requires
    more space, and halved when less than 25% is used. As a
    consequence, the time spent in enlarging/shrinking the array in a
    sequence of [M] insertions (resp. deletions) is proportional to [M],
    and thus be considered constant time for each operation. For this
    reason, we do not mention the possibility of a worst case complexity
    [O(n)] in operations {!val:add}/{!val:remove}/{!val:pop_minimum} below.

    This version is a de-functorized version of the bheap package from
    Jean-Christophe Filliatre. The `compare` function is passed via the
    {!val:create} function. *)

exception Empty

type 'a t
(** Type of priority queues. *)

val create : ?compare:('a -> 'a -> int) -> dummy:'a -> int -> 'a t
(** [create ?compare ~dummy c] creates a new heap, with initial capacity of [c].
    The value [dummy] is used to fill unused cells of the internal array.
    Note: [dummy] can still be used as a regular value in the queue. *)

val length : 'a t -> int
(** [length h] returns the number of elements of [h] *)

val is_empty : 'a t -> bool
(** [is_empty h] checks the emptiness of [h] *)

val add : 'a t -> 'a -> unit
(** [add x h] adds a new element [x] in heap [h]; complexity O(log(n)). *)

val minimum : 'a t -> 'a
(** [minimum h] returns the minimum element of [h]; raises [Empty]
    when [h] is empty; complexity O(1) *)

val remove : 'a t -> unit
(** [remove h] removes the minimum element of [h]; raises [Empty]
    when [h] is empty; complexity O(log(n)). *)

val pop_minimum : 'a t -> 'a
(** [pop_minimum h] removes the minimum element of [h] and returns it;
    raises [Empty] when [h] is empty; complexity O(log(n)). *)

val remove_and_add : 'a t -> 'a -> unit
(** [remove_and_add x h] removes the minimum element of [h] and adds [x];
    complexity O(log(n)). More efficient than calling [remove]
    and [add]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** usual iterators; elements are presented in arbitrary order *)
