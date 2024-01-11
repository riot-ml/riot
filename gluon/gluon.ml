module Sys = Sys
open Sys
module Poll = Poll

type 'rsc t = {
  poll : Poll.t;
  poll_timeout : Poll.Timeout.t;
  procs : (Fd.t, 'rsc * [ `r | `w | `rw ]) Hashtbl.t;
  fds : ('rsc, Fd.t) Hashtbl.t;
}

(* basic api over t *)

let create () =
  {
    poll = Poll.create ();
    poll_timeout = Poll.Timeout.after 1_000_000L;
    procs = Hashtbl.create 1024;
    fds = Hashtbl.create 1024;
  }

(* pretty-printing *)

let mode_to_string mode = match mode with `r -> "r" | `rw -> "rw" | `w -> "w"

let pp ppf t =
  Format.fprintf ppf "[";
  Hashtbl.iter
    (fun fd (_resource, mode) ->
      Format.fprintf ppf "%a:%s," Fd.pp fd (mode_to_string mode))
    t.procs;
  Format.fprintf ppf "]"

let event_of_mode mode =
  match mode with
  | `r -> Poll.Event.read
  | `rw -> Poll.Event.read_write
  | `w -> Poll.Event.write

let mode_of_event event =
  match event with
  | Poll.Event.{ writable = false; readable = true } -> Some `r
  | Poll.Event.{ writable = true; readable = true } -> Some `rw
  | Poll.Event.{ writable = true; readable = false } -> Some `w
  | Poll.Event.{ writable = false; readable = false } -> None

(* NOTE(leostera): when we add a new Fd.t to our collection here, we need
   to update the current poller so that it knows of it.

   If we don't call [add_fd t fd] then we will never poll on [fd].
*)
let add_fd t fd mode =
  if Fd.is_closed fd then ()
  else
    let unix_fd = Fd.get fd |> Option.get in
    let flags = event_of_mode mode in
    Poll.set t.poll unix_fd flags

let register t proc mode fd =
  add_fd t fd mode;
  Hashtbl.add t.procs fd (proc, mode);
  Hashtbl.add t.fds proc fd

let unregister t proc =
  let fds = Hashtbl.find_all t.fds proc in
  fds |> List.iter (Hashtbl.remove t.procs);
  List.iter
    (fun fd ->
      match Fd.get fd with
      | Some fd -> Poll.set t.poll fd Poll.Event.none
      | None -> ())
    fds;
  Hashtbl.remove t.fds proc

let can_poll t = Hashtbl.length t.procs > 0 && Hashtbl.length t.fds > 0

let poll t fn =
  match Poll.wait t.poll t.poll_timeout with
  | `Timeout -> ()
  | `Ok ->
      Poll.iter_ready t.poll ~f:(fun raw_fd event ->
          match mode_of_event event with
          | None -> ()
          | Some mode ->
              let fd = Fd.make raw_fd in
              let procs = Hashtbl.find_all t.procs fd in
              let mode_and_flag (_proc, mode') =
                let same_mode = Fd.Mode.equal mode' mode in
                match Fd.get fd with
                | Some fd' -> fd' = raw_fd && same_mode
                | _ -> false
              in
              procs |> List.filter mode_and_flag |> List.iter fn)

let close t fd =
  Hashtbl.remove t.procs fd;
  Fd.close fd

module Syscall = Syscall
