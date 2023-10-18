type fd = Unix.file_descr
type state = [ `Open of fd | `Closed ]
type t = state Rc.t

exception Fd_already_closed of string

let equal a b =
  match (Rc.peek a, Rc.peek b) with
  | `Open fd1, `Open fd2 -> Int.equal (Obj.magic fd1) (Obj.magic fd2)
  | _ -> false

let get t =
  match Rc.get t with
  | `Open sock -> Some sock
  | `Closed ->
      Rc.drop t;
      None

let to_int t = match Rc.peek t with `Open fd -> Obj.magic fd | `Closed -> -1
let pp fmt t = Format.fprintf fmt "Fd(%d,rc=%d)" (to_int t) (Rc.refc t)

let rec close t =
  Logs.trace (fun f -> f "close %a" pp t);
  match Rc.peek t with
  | `Closed -> ()
  | `Open fd as prev ->
      let next = `Closed in
      if Rc.set t ~prev ~next then Unix.close fd else close t

let make ~release fd =
  let release t =
    release t;
    close t;
    `Closed
  in
  Rc.make (`Open fd) ~release

let use ~op_name t fn =
  Rc.use t @@ function
  | `Open sock ->
      Logs.trace (fun f -> f "opening fd for %s: %a" op_name pp t);
      fn sock
  | `Closed -> raise (Fd_already_closed (op_name ^ ": fd already closed"))
