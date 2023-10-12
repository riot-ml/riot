module PidSet = Hashtbl.Make (struct
  type t = Pid.t

  let hash = Pid.hash
  let equal = Pid.equal
end)

type t = { mutable _set : unit PidSet.t }

let create () = { _set = PidSet.create 1024 }
let add t pid = PidSet.add t._set pid ()
let remove t pid = PidSet.remove t._set pid
let contains t pid = PidSet.mem t._set pid
let size t = PidSet.length t._set

let flush t =
  let pids = PidSet.to_seq_keys t._set |> List.of_seq in
  PidSet.clear t._set;
  pids
