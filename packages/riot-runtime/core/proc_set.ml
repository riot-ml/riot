module PidSet = Set.Make (struct
  type t = Process.t

  let compare (a : t) (b : t) = Pid.compare (Process.pid a) (Process.pid b)
end)

type t = { _set : PidSet.t Atomic.t } [@@unboxed]

let create () = { _set = Atomic.make PidSet.empty }

let rec remove t proc =
  let old_set = Atomic.get t._set in
  let new_set = PidSet.remove proc old_set in
  if Atomic.compare_and_set t._set old_set new_set then () else remove t proc

let contains t proc = PidSet.mem proc (Atomic.get t._set)
let size t = PidSet.cardinal (Atomic.get t._set)

(* NOTE(leostera): `PidSet.add` actually keeps duplicates! we want to use `replace` to drop the old one *)
let rec add t proc =
  let old_set = Atomic.get t._set in
  let new_set = PidSet.add proc old_set in
  if Atomic.compare_and_set t._set old_set new_set then () else add t proc
