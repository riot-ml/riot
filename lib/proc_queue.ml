type t = { mutable queue : Process.t Lf_queue.t; size : int Atomic.t }

let create () = { queue = Lf_queue.create (); size = Atomic.make 0 }
let clear t = t.queue <- Lf_queue.create ()
let is_empty t = Lf_queue.is_empty t.queue
let length t = Atomic.get t.size

let queue t pid =
  Atomic.incr t.size;
  Lf_queue.add pid t.queue

let next t =
  Atomic.decr t.size;
  Lf_queue.take_opt t.queue
