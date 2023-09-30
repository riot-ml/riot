(* (c) Lwt authors *)
[@@@warning "-69"]

type 'a t = { g : Random.State.t; mutable prev : 'a t; mutable next : 'a t }

type 'a node = {
  g : Random.State.t;
  mutable prev : 'a t;
  mutable next : 'a t;
  mutable data : 'a;
  mutable active : bool;
}

external t_of_node : 'a node -> 'a t = "%identity"
external node_of_t : 'a t -> 'a node = "%identity"

exception Empty

let create g =
  let rec t = { g; prev = t; next = t } in
  t

let remove node =
  if node.active then (
    node.active <- false;
    node.data <- Obj.magic ();
    let t = t_of_node node in
    t.prev.next <- t.next;
    t.next.prev <- t.prev)

let is_empty (t : 'a t) = t.next == t
let data { data; _ } = data

let add_l data (t : 'a t) =
  let node = { g = t.g; prev = t; next = t.next; data; active = true } in
  t.next.prev <- t_of_node node;
  t.next <- t_of_node node

let add_r data (t : 'a t) =
  let node = { g = t.g; prev = t.prev; next = t; data; active = true } in
  t.prev.next <- t_of_node node;
  t.prev <- t_of_node node

let push data (t : 'a t) =
  if Random.State.bool t.g then add_l data t else add_r data t

let length t =
  let rec go curr len =
    if curr == t then len
    else
      let node = node_of_t curr in
      go node.next (len + 1)
  in
  go t.next 0

(* NOTE(dinosaure): [take_{r,l}] are unsafe. *)

let take_l (t : 'a t) =
  let node = node_of_t t.next in
  remove node;
  node.data

let drop t =
  while not (is_empty t) do
    ignore (take_l t)
  done

let take_r (t : 'a t) =
  let node = node_of_t t.prev in
  remove node;
  node.data

let take t =
  if is_empty t then raise Empty
  else if Random.State.bool t.g then take_r t
  else take_l t

let take_r t = if is_empty t then raise Empty else take_r t
let take_l t = if is_empty t then raise Empty else take_l t

let iter ~f t =
  let rec go cur =
    if cur != t then (
      let node = node_of_t cur in
      if node.active then f node.data;
      go node.next)
  in
  go t.next

let iter_node ~f t =
  let rec go cur =
    if cur != t then (
      let node = node_of_t cur in
      if node.active then f node;
      go node.next)
  in
  go t.next

let to_list t =
  let res = ref [] in
  let f data = res := data :: !res in
  iter ~f t;
  List.rev !res
