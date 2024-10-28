type t = {
  rnd : Random.State.t;
  max_workers : int;
  workers : int;
  supervisor_restart_limit : int;
  supervisor_restart_period : int;
}

let pp fmt t = 
  Format.fprintf fmt "== RIOT CONFIG ==\n";
  Format.fprintf fmt "* max_wokers=%d\n" t.max_workers;
  Format.fprintf fmt "* workers=%d\n" t.workers;
  Format.fprintf fmt "* supervisor_restart_limit=%d\n" t.supervisor_restart_limit;
  Format.fprintf fmt "* supervisor_restart_period=%d\n" t.supervisor_restart_period;
  Format.fprintf fmt "\n%!"
;;

let make ?(supervisor_restart_limit = 1) ?(supervisor_restart_period = 0)
    ?workers () =
  let max_workers = Int.max 0 (Stdlib.Domain.recommended_domain_count () - 2) in
  let workers =
    match workers with Some w -> Int.min w max_workers | None -> max_workers
  in
  let rnd = Random.State.make_self_init () in
  {
    rnd;
    max_workers;
    workers;
    supervisor_restart_limit;
    supervisor_restart_period;
  }

let default () = make ()
