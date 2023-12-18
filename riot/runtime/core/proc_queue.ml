open Util

type t = { alive : Proc_set.t; queue : Process.t Lf_queue.t }

let create () = { queue = Lf_queue.create (); alive = Proc_set.create () }
let size t = Proc_set.size t.alive
let is_empty t = size t = 0

let queue t proc =
  if Proc_set.contains t.alive proc then ()
  else (
    Proc_set.add t.alive proc;
    Lf_queue.push t.queue proc)

let next t =
  match Lf_queue.pop t.queue with
  | Some proc ->
      Proc_set.remove t.alive proc;
      Some proc
  | None -> None
