type t = { queue : Process.t Lf_queue.t } [@@unboxed]

let create () = { queue = Lf_queue.create () }
let is_empty t = Lf_queue.is_empty t.queue
let queue t pid = Lf_queue.push t.queue pid
let next t = Lf_queue.pop t.queue
