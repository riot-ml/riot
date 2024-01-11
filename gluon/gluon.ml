module Sys = Sys
open Sys

[@@@warning "-32"]

module Dashmap = struct
  type ('k, 'v) t = { tbl : ('k, 'v) Hashtbl.t; lock : Mutex.t }

  let create ?(size = 1024) () =
    { lock = Mutex.create (); tbl = Hashtbl.create size }

  let get_all t k = Mutex.protect t.lock (fun () -> Hashtbl.find_all t.tbl k)
  let get t k = Mutex.protect t.lock (fun () -> Hashtbl.find_opt t.tbl k)
  let remove t k = Mutex.protect t.lock (fun () -> Hashtbl.remove t.tbl k)

  let remove_all t ks =
    Mutex.protect t.lock (fun () -> List.iter (Hashtbl.remove t.tbl) ks)

  let entries t =
    Mutex.protect t.lock (fun () -> Hashtbl.to_seq t.tbl |> List.of_seq)

  let find_by t fn =
    Mutex.protect t.lock (fun () -> Hashtbl.to_seq t.tbl |> Seq.find fn)

  let find_all_by t fn =
    Mutex.protect t.lock (fun () ->
        Hashtbl.to_seq t.tbl |> Seq.filter fn |> List.of_seq)

  let iter t fn = Hashtbl.iter (fun k v -> fn (k, v)) t.tbl
  let has_key t k = Mutex.protect t.lock (fun () -> Hashtbl.mem t.tbl k)
  let is_empty t = Mutex.protect t.lock (fun () -> Hashtbl.length t.tbl = 0)

  let insert t k v =
    Mutex.protect t.lock (fun () -> Hashtbl.add t.tbl k v |> ignore)

  let remove_by t fn =
    Mutex.protect t.lock (fun () ->
        Hashtbl.to_seq t.tbl |> Seq.filter fn
        |> Seq.map (fun (k, _v) -> k)
        |> Seq.iter (fun k -> Hashtbl.remove t.tbl k))

  let replace t k v = Mutex.protect t.lock (fun () -> Hashtbl.replace t.tbl k v)

  let pp k_pp fmt t =
    Format.pp_print_list
      ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
      (fun fmt (k, _) -> k_pp fmt k)
      fmt (entries t)

  module type Base = sig
    type key

    val hash : key -> int
    val equal : key -> key -> bool
  end

  module type Intf = sig
    type key
    type 'v t

    val create : ?size:int -> unit -> 'v t
    val keys : 'v t -> key Seq.t
    val get : 'v t -> key -> 'v option
    val get_all : 'v t -> key -> 'v list
    val is_empty : 'v t -> bool
    val find_by : 'v t -> (key * 'v -> bool) -> (key * 'v) option
    val remove : 'v t -> key -> unit
    val remove_all : 'v t -> key list -> unit
    val find_all_by : 'v t -> (key * 'v -> bool) -> (key * 'v) list
    val has_key : 'v t -> key -> bool
    val insert : 'v t -> key -> 'v -> unit
    val remove_by : 'v t -> (key * 'v -> bool) -> unit
    val replace : 'v t -> key -> 'v -> unit
    val iter : 'v t -> (key * 'v -> unit) -> unit

    val pp :
      (Format.formatter -> key -> unit) -> Format.formatter -> 'v t -> unit
  end

  module Make (B : Base) : Intf with type key = B.key = struct
    module Hashtbl = Hashtbl.Make (struct
      type t = B.key

      let hash = B.hash
      let equal = B.equal
    end)

    type key = B.key
    type 'v t = { tbl : 'v Hashtbl.t; lock : Mutex.t }

    let keys t = Hashtbl.to_seq_keys t.tbl

    let create ?(size = 1024) () =
      { lock = Mutex.create (); tbl = Hashtbl.create size }

    let get_all t k = Mutex.protect t.lock (fun () -> Hashtbl.find_all t.tbl k)
    let get t k = Mutex.protect t.lock (fun () -> Hashtbl.find_opt t.tbl k)
    let remove t k = Mutex.protect t.lock (fun () -> Hashtbl.remove t.tbl k)

    let remove_all t ks =
      Mutex.protect t.lock (fun () -> List.iter (Hashtbl.remove t.tbl) ks)

    let entries t =
      Mutex.protect t.lock (fun () -> Hashtbl.to_seq t.tbl |> List.of_seq)

    let find_by t fn =
      Mutex.protect t.lock (fun () -> Hashtbl.to_seq t.tbl |> Seq.find fn)

    let find_all_by t fn =
      Mutex.protect t.lock (fun () ->
          Hashtbl.to_seq t.tbl |> Seq.filter fn |> List.of_seq)

    let iter t fn = Hashtbl.iter (fun k v -> fn (k, v)) t.tbl
    let has_key t k = Mutex.protect t.lock (fun () -> Hashtbl.mem t.tbl k)
    let is_empty t = Mutex.protect t.lock (fun () -> Hashtbl.length t.tbl = 0)

    let insert t k v =
      Mutex.protect t.lock (fun () -> Hashtbl.add t.tbl k v |> ignore)

    let remove_by t fn =
      Mutex.protect t.lock (fun () ->
          Hashtbl.to_seq t.tbl |> Seq.filter fn
          |> Seq.map (fun (k, _v) -> k)
          |> Seq.iter (fun k -> Hashtbl.remove t.tbl k))

    let replace t k v =
      Mutex.protect t.lock (fun () -> Hashtbl.replace t.tbl k v)

    let pp k_pp fmt t =
      Format.pp_print_list
        ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
        (fun fmt (k, _) -> k_pp fmt k)
        fmt (entries t)
  end
end

module Poll = Poll

type 'rsc t = {
  poll : Poll.t;
  poll_timeout : Poll.Timeout.t;
  procs : (Fd.t, 'rsc * [ `r | `w | `rw ]) Dashmap.t;
  fds : ('rsc, Fd.t) Dashmap.t;
}

let create () =
  {
    poll = Poll.create ();
    poll_timeout = Poll.Timeout.after 1_000_000L;
    procs = Dashmap.create ();
    fds = Dashmap.create ();
  }

let mode_to_string mode = match mode with `r -> "r" | `rw -> "rw" | `w -> "w"

let pp ppf t =
  Format.fprintf ppf "[";
  Dashmap.iter t.procs (fun (fd, (_resource, mode)) ->
      Format.fprintf ppf "%a:%s," Fd.pp fd (mode_to_string mode));
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
  Dashmap.insert t.procs fd (proc, mode);
  Dashmap.insert t.fds proc fd

let unregister t proc =
  let fds = Dashmap.get_all t.fds proc in
  fds |> Dashmap.remove_all t.procs;
  List.iter
    (fun fd ->
      match Fd.get fd with
      | Some fd -> Poll.set t.poll fd Poll.Event.none
      | None -> ())
    fds;
  Dashmap.remove t.fds proc

let can_poll t =
  (not (Dashmap.is_empty t.procs)) && not (Dashmap.is_empty t.fds)

let poll t fn =
  Poll.clear t.poll;
  match Poll.wait t.poll t.poll_timeout with
  | `Timeout -> ()
  | `Ok ->
      Poll.iter_ready t.poll ~f:(fun raw_fd event ->
          match mode_of_event event with
          | None -> ()
          | Some mode ->
              let fd = Fd.make raw_fd in
              let procs = Dashmap.get_all t.procs fd in
              let mode_and_flag (_proc, mode') =
                let same_mode = Fd.Mode.equal mode' mode in
                match Fd.get fd with
                | Some fd' -> fd' = raw_fd && same_mode
                | _ -> false
              in
              procs |> List.filter mode_and_flag |> List.iter fn)

let close t fd =
  Dashmap.remove t.procs fd;
  Fd.close fd

module Syscall = Syscall
