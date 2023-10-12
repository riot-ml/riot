type t = { queue : Pid.t Lf_queue.t } [@@unboxed]

let create () = { queue = Lf_queue.create () }
let queue t pid = Lf_queue.add pid t.queue
let is_empty t = Lf_queue.is_empty t.queue
let next t = Lf_queue.take_opt t.queue
