type ('k, 'v) t = { tbl : ('k * 'v) list Atomic.t } [@@unboxed]

let create _size = { tbl = Atomic.make [] }
let entries t = Atomic.get t.tbl
let find t k = List.assoc_opt k (entries t)
let find_by t fn = List.find_opt fn (entries t)
let find_all_by t fn = List.find_all fn (entries t)
let iter t fn = List.iter fn (entries t)
let has_key t k = find t k |> Option.is_some
let is_empty t = entries t = []

let rec insert t k v =
  let tbl1 = entries t in
  let entry = (k, v) in
  if List.mem entry tbl1 then ()
  else
    let tbl2 = (k, v) :: tbl1 in
    if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else insert t k v

let rec remove_by t fn =
  let tbl1 = entries t in
  let tbl2 = List.filter fn tbl1 in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else remove_by t fn

let rec replace t k v =
  let tbl1 = entries t in
  let tbl2 = (k, v) :: List.remove_assoc k tbl1 in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else replace t k v

let pp k_pp fmt t =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
    (fun fmt (k, _) -> k_pp fmt k)
    fmt (entries t)
