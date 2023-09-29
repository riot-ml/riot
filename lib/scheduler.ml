type t = { id : int; domains : int list; run_procs : unit Miou.t Miou.Queue.t }

let empty = { domains = []; run_procs = Miou.Queue.create (); id = 0 }
let make ~id = { empty with id }
let __SHARED_PROC_QUEUE__ = Miou.Queue.create ()

let spawn fn =
  let pid = Process.make fn in
  let pack = Process.Pack (pid, Process.recv pid) in
  Miou.Queue.enqueue __SHARED_PROC_QUEUE__ pack;
  pid

let run_scheduler t () =
  let spawn_task =
    Miou.call_cc @@ fun () ->
    let rec run () =
      match Miou.Queue.dequeue __SHARED_PROC_QUEUE__ with
      | exception _ ->
          Miou.yield ();
          run ()
      | proc ->
          let miou_task =
            Miou.call_cc (fun () ->
                Miou.yield ();
                Process.run proc)
          in
          Miou.Queue.enqueue t.run_procs miou_task;
          Miou.yield ();
          run ()
    in
    run ()
  in
  let rec run () =
    Miou.await_first (spawn_task :: Miou.Queue.to_list t.run_procs)
    |> Result.get_ok;
    run ()
  in
  run ()

let run () =
  List.init (Domain.recommended_domain_count ()) @@ fun id ->
  let t = make ~id in
  Miou.call @@ run_scheduler t
