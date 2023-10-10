type t = { processes : (Pid.t, Process.t) Hashtbl.t; lock : Mutex.t }

let create () = { lock = Mutex.create (); processes = Hashtbl.create 16_000 }

let register_process t (proc : Process.t) =
  Mutex.lock t.lock;
  Hashtbl.add t.processes proc.pid proc;
  Mutex.unlock t.lock

let get t pid = Hashtbl.find_opt t.processes pid
let process_count t = Hashtbl.length t.processes
let processes t = Hashtbl.to_seq t.processes
