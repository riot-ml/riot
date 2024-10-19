type t = { processes : (Pid.t, Process.t) Hashtbl.t; lock : Mutex.t }

let create () = { lock = Mutex.create (); processes = Hashtbl.create 16_000 }
let get t pid = Hashtbl.find_opt t.processes pid
let remove t pid = Hashtbl.remove t.processes pid
let size t = Hashtbl.length t.processes

exception Reregistering_process of Process.t

let register_process t (proc : Process.t) =
  Mutex.protect t.lock @@ fun () ->
  let pid = Process.pid proc in
  if Hashtbl.mem t.processes pid then raise (Reregistering_process proc)
  else Hashtbl.replace t.processes pid proc

let processes t = Hashtbl.to_seq t.processes
