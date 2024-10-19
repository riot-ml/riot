type t = { rnd : Random.State.t; max_workers : int; workers : int }

let make ?workers () =
  let max_workers = Int.max 0 (Stdlib.Domain.recommended_domain_count () - 2) in
  let workers =
    match workers with Some w -> Int.min w max_workers | None -> max_workers
  in
  let rnd = Random.State.make_self_init () in
  { rnd; max_workers; workers }

let default () = make ()
