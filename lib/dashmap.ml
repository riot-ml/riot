type ('k, 'v) t = { tbl : ('k * 'v) list Atomic.t } [@@unboxed]

let create _size = { tbl = Atomic.make [] }
let entries t = Atomic.get t.tbl
let keys t = List.map (fun (k, _) -> k) (entries t)
let values t = List.map (fun (_, v) -> v) (entries t)
let find t k = List.assoc_opt k (entries t)

let rec insert t k v =
  let tbl1 = entries t in
  let tbl2 = (k, v) :: tbl1 in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else insert t k v

let rec remove t pid fd =
  let tbl1 = entries t in
  let tbl2 =
    List.filter (fun (pid', fd') -> not (Pid.equal pid pid' && fd = fd')) tbl1
  in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else remove t pid fd

let rec replace t k v =
  let tbl1 = entries t in
  let tbl2 = (k, v) :: List.remove_assoc k tbl1 in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else replace t k v

let rec update t fn =
  let tbl1 = entries t in
  let tbl2 = List.filter_map fn tbl1 in
  if Atomic.compare_and_set t.tbl tbl1 tbl2 then () else update t fn
