open Util

module Lf_queue = struct
  type 'a t = 'a Queue.t

  let create () = Queue.create ()
  let push q el = Queue.push el q
  let pop q = Queue.take_opt q
end

type priority_queues = {
  high : Process.t Weak_ref.t Lf_queue.t;
  normal : Process.t Weak_ref.t Lf_queue.t;
  low : Process.t Weak_ref.t Lf_queue.t;
}

let make_priority_queues () =
  {
    high = Lf_queue.create ();
    normal = Lf_queue.create ();
    low = Lf_queue.create ();
  }

type t = { alive : Proc_set.t; queue : priority_queues; lock : Mutex.t }

let create () =
  {
    queue = make_priority_queues ();
    alive = Proc_set.create ();
    lock = Mutex.create ();
  }

let size t = Proc_set.size t.alive
let is_empty t = size t = 0

let rec queue t proc =
  if Mutex.try_lock t.lock then (
    if Proc_set.contains t.alive proc then ()
    else (
      Proc_set.add t.alive proc;
      let wref = Weak_ref.make proc in
      match Atomic.get proc.flags.priority with
      | High -> Lf_queue.push t.queue.high wref
      | Normal -> Lf_queue.push t.queue.normal wref
      | Low -> Lf_queue.push t.queue.low wref);
    Mutex.unlock t.lock)
  else queue t proc

let do_pop t queue =
  match Lf_queue.pop queue with
  | Some proc -> (
      match Weak_ref.get proc with
      | Some proc when Proc_set.contains t.alive proc ->
          Proc_set.remove t.alive proc;
          Some proc
      | _ -> None)
  | None -> None

let next t =
  if Mutex.try_lock t.lock then (
    let proc =
      match do_pop t t.queue.high with
      | Some proc -> Some proc
      | None -> (
          match do_pop t t.queue.normal with
          | Some proc -> Some proc
          | None -> do_pop t t.queue.low)
    in
    Mutex.unlock t.lock;
    proc)
  else None

let remove t proc = Proc_set.remove t.alive proc
