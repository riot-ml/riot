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
