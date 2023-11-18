type t = { processes : (Pid.t, Process.t) Hashtbl.t; lock : Mutex.t }

let create () = { lock = Mutex.create (); processes = Hashtbl.create 16_000 }
let get t pid = Hashtbl.find_opt t.processes pid

exception Reregistering_process of Process.t

let register_process t (proc : Process.t) =
  Mutex.lock t.lock;
  let pid = Process.pid proc in
  if Hashtbl.mem t.processes pid then raise (Reregistering_process proc)
  else Hashtbl.add t.processes pid proc;
  Mutex.unlock t.lock

let processes t = Hashtbl.to_seq t.processes
