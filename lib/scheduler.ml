type t = { id : int; domains : int list; run_procs : unit Miou.t Miou.Queue.t }

let empty = { domains = []; run_procs = Miou.Queue.create (); id = 0 }
let make ~id = { empty with id }
let __SHARED_PROC_QUEUE__ = Miou.Queue.create ()

let __SHARED_PROC_MAP__ :
    (Process.pid, Process.pack) Hashtbl.t =
  Hashtbl.create 1024

let spawn fn =
  let proc = Process.make fn in
  let pack = Process.Pack (proc, Process.recv proc) in
  Hashtbl.add __SHARED_PROC_MAP__ proc.pid pack;
  Miou.Queue.enqueue __SHARED_PROC_QUEUE__ pack;
  proc

let find_process pid = 
  Hashtbl.find_opt __SHARED_PROC_MAP__ pid

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
                let dead_pid, monitors, links = Process.run proc in

                (* notify monitors that we have died *)
                monitors
                |> List.iter (fun pid ->
                       let process = find_process pid |> Option.get in
                       Process.signal process
                         (Process.Monitor_process_died dead_pid));

                (* bring down any linked processes *)
                links
                |> List.iter (fun pid ->
                    (* NOTE(leostera): here's where not being able to interrupt
                       the execution of a fiber gets in the way.
                       to do links, we will need to go one level deeper.
                     *)
                    ())

                )
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
