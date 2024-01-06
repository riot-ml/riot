module Mode = struct
  type t = [ `r | `rw | `w ]

  let equal a b =
    match (a, b) with
    | `r, `r -> true
    | `w, `w -> true
    | `rw, `rw -> true
    | _ -> false

  let pp fmt t =
    let mode = match t with `r -> "r" | `w -> "w" | `rw -> "rw" in
    Format.fprintf fmt "%s" mode
end

type fd = Unix.file_descr
type state = [ `Open of fd | `Closed ]
type t = { state : state Atomic.t }

exception Already_closed of string

let is_closed t = Atomic.get t.state = `Closed
let is_open t = not (is_closed t)

let get t =
  match Atomic.get t.state with `Open sock -> Some sock | `Closed -> None

let equal a b =
  match (get a, get b) with
  | Some fd1, Some fd2 -> Int.equal (Obj.magic fd1) (Obj.magic fd2)
  | _ -> false

let to_int t =
  match Atomic.get t.state with `Open fd -> Obj.magic fd | `Closed -> -1

let pp fmt t =
  if is_closed t then Format.fprintf fmt "Fd.closed(%d)" (to_int t)
  else Format.fprintf fmt "Fd(%d)" (to_int t)

let rec close t =
  match Atomic.get t.state with
  | `Closed ->
      Log.error (fun f -> f "closing socket %a" pp t);
      ()
  | `Open fd as prev ->
      let next = `Closed in
      if Atomic.compare_and_set t.state prev next then Unix.close fd
      else close t

let make fd = { state = Atomic.make (`Open fd) }

let use ~op_name t fn =
  match get t with
  | Some sock -> fn sock
  | None -> raise (Already_closed (op_name ^ ": fd already closed"))

let seek t pos command =
  match get t with
  | Some sock -> Some (Unix.lseek sock pos command)
  | None -> None

module Set = Set.Make (struct
  type nonrec t = t

  let compare a b = if equal a b then 0 else 1
end)
