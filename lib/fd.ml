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
type t = { state : state Atomic.t; fd : fd }

exception Fd_already_closed of string

let is_closed t = Atomic.get t.state = `Closed
let is_open t = not (is_closed t)

let get t =
  match Atomic.get t.state with `Open sock -> Some sock | `Closed -> None

let equal a b =
  match (get a, get b) with
  | Some fd1, Some fd2 -> Int.equal (Obj.magic fd1) (Obj.magic fd2)
  | _ -> false

let to_int t =
  match Atomic.get t.state with
  | `Open fd -> Obj.magic fd
  | `Closed -> Obj.magic t.fd

let pp fmt t =
  if is_closed t then Format.fprintf fmt "Fd.closed(%d)" (to_int t)
  else Format.fprintf fmt "Fd(%d)" (to_int t)

let rec close t =
  Logs.trace (fun f -> f "close %a" pp t);
  match Atomic.get t.state with
  | `Closed -> ()
  | `Open fd as prev ->
      let next = `Closed in
      if Atomic.compare_and_set t.state prev next then Unix.close fd
      else close t

let make fd = { state = Atomic.make (`Open fd); fd }

let use ~op_name t fn =
  match get t with
  | Some sock -> fn sock
  | None -> raise (Fd_already_closed (op_name ^ ": fd already closed"))
