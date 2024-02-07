open Gluon_common
open Gluon_sys
open Rio

type t = Fd.t

let pp = Fd.pp
let close = Fd.close

let read fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.read fd ~buf ~pos ~len)

let write fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.write fd ~buf ~pos ~len)

external gluon_readv : Unix.file_descr -> Iovec.t -> int = "gluon_unix_readv"

let read_vectored fd iov = syscall @@ fun () -> Ok (gluon_readv fd iov)

external gluon_writev : Unix.file_descr -> Iovec.t -> int = "gluon_unix_writev"

let write_vectored fd iov = syscall @@ fun () -> Ok (gluon_writev fd iov)

external gluon_sendfile :
  Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "gluon_unix_sendfile"

let sendfile fd ~file ~off ~len =
  syscall @@ fun () -> Ok (gluon_sendfile file fd off len)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register t selector token interest =
      Sys.Selector.register selector ~fd:t ~token ~interest

    let reregister t selector token interest =
      Sys.Selector.reregister selector ~fd:t ~token ~interest

    let deregister t selector = Sys.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
