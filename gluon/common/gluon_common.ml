type io_error =
  [ `Exn of exn | `Would_block | `Unix_error of Unix.error | `No_info ]

type 'ok io_result = ('ok, [ | io_error ]) result

let ( let* ) = Result.bind

module Token = Token

let rec syscall fn =
  match fn () with
  | ok -> Ok ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      Error `Would_block
  | exception Unix.(Unix_error (reason, _, _)) -> Error (`Unix_error reason)

module Fd = struct
  type t = Unix.file_descr

  let make fd = fd
  let pp fmt t = Format.fprintf fmt "Fd(%d)" (Obj.magic t)
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
