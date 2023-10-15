type t = { size : int Atomic.t; queue : Message.envelope Lf_queue.t }

let create () = { size = Atomic.make 0; queue = Lf_queue.create () }

let queue t msg =
  Atomic.incr t.size;
  Lf_queue.push t.queue msg

let next (t : t) =
  match Lf_queue.pop t.queue with
  | Some msg ->
      Atomic.decr t.size;
      Some msg
  | None ->
      Atomic.set t.size 0;
      None

let size (t : t) = Atomic.get t.size
let is_empty (t : t) = Lf_queue.is_empty t.queue
