let ( let* ) = Result.bind
let log = Format.printf

module Token = Token

let rec syscall fn =
  match fn () with
  | ok -> Ok ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      (* log "syscall is try again\n"; *)
      Error `Would_block
  | exception Unix.(Unix_error (reason, _, _)) -> Error (`Unix_error reason)

module Fd = struct
  type t = Unix.file_descr

  let to_int fd = Obj.magic fd
  let make fd = fd
  let pp fmt t = Format.fprintf fmt "Fd(%d)" (Obj.magic t)
  let close t = Unix.close t
  let seek = Unix.lseek
  let equal a b = Int.equal (to_int a) (to_int b)
end

module Non_zero_int = struct
  type t = int

  let make a = if a > 0 then Some a else None
end

module Interest : sig
  type t

  val readable : t
  val writable : t
  val add : t -> t -> t
  val remove : t -> t -> t option
  val is_readable : t -> bool
  val is_writable : t -> bool
end = struct
  type t = Non_zero_int.t

  let readable = 0b0001
  let writable = 0b0010
  let add a b = a lor b
  let remove a b = Non_zero_int.make (a land lnot b)
  let is_readable t = t land readable != 0
  let is_writable t = t land writable != 0
end
