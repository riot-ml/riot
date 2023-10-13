type 'a t = {
  refc : int Atomic.t;
  value : 'a Atomic.t;
  release : 'a t -> 'a;
      (** this release function will be called once we are sure the last reference
      to this resource is dropped.

      note that the entire instance is passed to this release function
      to allow for more uniform APIs. *)
}

let make value ~release =
  { refc = Atomic.make 1; value = Atomic.make value; release }

let set t ~prev ~next = Atomic.compare_and_set t.value prev next
let refc t = Atomic.get t.refc

let get t =
  Atomic.incr t.refc;
  Atomic.get t.value

let peek t = Atomic.get t.value

let release t =
  let new_value = t.release t in
  Atomic.set t.value new_value

let take t = Atomic.incr t.refc

let drop t =
  let old_refc = Atomic.fetch_and_add t.refc (-1) in
  if old_refc = 1 then release t else assert (old_refc > 1)

let use t fn =
  (* bump refc: so noone can close the resource while fn runs *)
  take t;
  match fn (Atomic.get t.value) with
  | value ->
      drop t;
      value
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      drop t;
      Printexc.raise_with_backtrace exn backtrace
