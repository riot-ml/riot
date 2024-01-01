open Util

type priority_queues = {
  high : Process.t Lf_queue.t;
  normal : Process.t Lf_queue.t;
  low : Process.t Lf_queue.t;
}

let make_priority_queues () =
  {
    high = Lf_queue.create ();
    normal = Lf_queue.create ();
    low = Lf_queue.create ();
  }

type t = { alive : Proc_set.t; queue : priority_queues }

let create () = { queue = make_priority_queues (); alive = Proc_set.create () }
let size t = Proc_set.size t.alive
let is_empty t = size t = 0

let queue t proc =
  if Proc_set.contains t.alive proc then ()
  else (
    Proc_set.add t.alive proc;
    match Atomic.get proc.flags.priority with
    | High -> Lf_queue.push t.queue.high proc
    | Normal -> Lf_queue.push t.queue.normal proc
    | Low -> Lf_queue.push t.queue.low proc)

let next t =
  let queue =
    match
      ( Lf_queue.is_empty t.queue.high,
        Lf_queue.is_empty t.queue.normal,
        Lf_queue.is_empty t.queue.low )
    with
    | false, _, _ -> t.queue.high
    | _, false, _ -> t.queue.normal
    | _, _, _ -> t.queue.low
  in
  match Lf_queue.pop queue with
  | Some proc ->
      Proc_set.remove t.alive proc;
      Some proc
  | None -> None
