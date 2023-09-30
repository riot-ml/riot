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

exception Empty

type 'a t = {
  mutable size : int;
  mutable data : 'a array;
  dummy : 'a;
  compare : 'a -> 'a -> int;
  min_cap : int; (* minimal capacity, as given initially *)
}
(* invariant 0 <= size <= length data *)
(* invariant data[size..] only contains dummy *)

let create ?(compare = Stdlib.compare) ~dummy n =
  if n < 0 || n > Sys.max_array_length then invalid_arg "create";
  let n = max 16 n in
  { size = 0; data = Array.make n dummy; dummy; compare; min_cap = n }

let length h = h.size
let is_empty h = h.size = 0

(* [enlarge] doubles the size of [data] *)
let enlarge h =
  let n = h.size in
  assert (n > 0 && n = Array.length h.data);
  let n' = min (2 * n) Sys.max_array_length in
  if n' = n then failwith "maximum capacity reached";
  let d = h.data in
  let d' = Array.make n' h.dummy in
  Array.blit d 0 d' 0 n;
  h.data <- d'

let shrink h =
  let n = Array.length h.data in
  let n' = max h.min_cap (n / 2) in
  assert (h.size <= n' && n' <= n);
  if n' < n then (
    let d = h.data in
    let d' = Array.make n' h.dummy in
    Array.blit d 0 d' 0 h.size;
    h.data <- d')

let add h x =
  let n = h.size in
  if n == Array.length h.data then enlarge h;
  let d = h.data in
  let rec moveup i =
    let fi = (i - 1) / 2 in
    if i > 0 && h.compare d.(fi) x > 0 then (
      d.(i) <- d.(fi);
      moveup fi)
    else d.(i) <- x
  in
  moveup n;
  h.size <- n + 1

let minimum h =
  if h.size <= 0 then raise Empty;
  h.data.(0)

let rec movedown ~compare d n i x =
  let j = (2 * i) + 1 in
  if j < n then
    let j =
      let j' = j + 1 in
      if j' < n && compare d.(j') d.(j) < 0 then j' else j
    in
    if compare d.(j) x < 0 then (
      d.(i) <- d.(j);
      movedown ~compare d n j x)
    else d.(i) <- x
  else d.(i) <- x

let remove h =
  if h.size <= 0 then raise Empty;
  let n = h.size - 1 in
  h.size <- n;
  let d = h.data in
  let x = d.(n) in
  d.(n) <- h.dummy;
  movedown ~compare:h.compare d n 0 x;
  if 4 * h.size < Array.length h.data then shrink h

let remove_and_add h x =
  if h.size = 0 then add h x else movedown ~compare:h.compare h.data h.size 0 x

let pop_minimum h =
  let m = minimum h in
  remove h;
  m

let iter f h =
  let d = h.data in
  for i = 0 to h.size - 1 do
    f d.(i)
  done
