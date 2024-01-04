type ('k, 'v) t = { tbl : ('k, 'v) Hashtbl.t; lock : Mutex.t }

let create size = { lock = Mutex.create (); tbl = Hashtbl.create size }
let get t k = Mutex.protect t.lock (fun () -> Hashtbl.find_all t.tbl k)
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

  val create : int -> 'v t
  val keys : 'v t -> key Seq.t
  val get : 'v t -> key -> 'v list
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
  val pp : (Format.formatter -> key -> unit) -> Format.formatter -> 'v t -> unit
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
  let create size = { lock = Mutex.create (); tbl = Hashtbl.create size }
  let get t k = Mutex.protect t.lock (fun () -> Hashtbl.find_all t.tbl k)
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
end
