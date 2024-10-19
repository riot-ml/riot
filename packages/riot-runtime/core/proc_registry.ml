module Exn = struct
  exception Name_already_registered of string * Pid.t
end

type t = {
  processes : (string, Pid.t) Hashtbl.t;
  names : (Pid.t, string) Hashtbl.t;
  lock : Mutex.t;
}

let create () =
  {
    lock = Mutex.create ();
    processes = Hashtbl.create 16_000;
    names = Hashtbl.create 16_000;
  }

let register t name pid =
  Mutex.lock t.lock;
  if Hashtbl.mem t.processes name then (
    Mutex.unlock t.lock;
    raise (Exn.Name_already_registered (name, pid)))
  else (
    Hashtbl.add t.processes name pid;
    Hashtbl.add t.names pid name;
    Mutex.unlock t.lock)

let unregister t name =
  Mutex.lock t.lock;
  let pid = Hashtbl.find t.processes name in
  Hashtbl.remove t.processes name;
  Hashtbl.remove t.names pid;
  Mutex.unlock t.lock

let remove t pid =
  Mutex.lock t.lock;
  (match Hashtbl.find_opt t.names pid with
  | Some name -> Hashtbl.remove t.processes name
  | None -> ());
  Hashtbl.remove t.names pid;
  Mutex.unlock t.lock

let find_pid t name =
  Mutex.lock t.lock;
  let pid = Hashtbl.find_opt t.processes name in
  Mutex.unlock t.lock;
  pid
