type t = { size : int Atomic.t; queue : Message.t Lf_queue.t }

let create () = { size = Atomic.make 0; queue = Lf_queue.create () }

let queue t msg =
  Atomic.incr t.size;
  Lf_queue.add msg t.queue

let next (t : t) =
  Atomic.decr t.size;
  Lf_queue.take_opt t.queue

let is_empty (t : t) = Lf_queue.is_empty t.queue
let merge (a : t) (b : t) = Lf_queue.merge a.queue b.queue
let size (t : t) = Atomic.get t.size
