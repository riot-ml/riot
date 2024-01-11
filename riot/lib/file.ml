open Global

type 'kind file = { fd : Fd.t; path : string }
type read_file = [ `r ] file
type write_file = [ `w ] file
type rw_file = [ `w | `r ] file

let fd t = t.fd
let base_permissions = 0o640

let do_open path flags =
  let raw_fd = Unix.openfile path flags base_permissions in
  { fd = Fd.make raw_fd; path }

let open_read path = do_open path Unix.[ O_RDONLY ]
let open_write path = do_open path Unix.[ O_WRONLY; O_CREAT ]
let close t = Fd.close t.fd
let remove path = Unix.unlink path
let seek t ~off = Fd.seek t.fd off Unix.SEEK_SET
let stat path = Unix.stat path

let rec send ?(off = 0) ~len file socket =
  match Gluon.Syscall.sendfile file.fd socket ~off ~len with
  | `Abort reason -> Error (`Unix_error reason)
  | `Retry ->
      syscall "receive" `r socket @@ fun socket -> send ~off ~len file socket
  | `Sent 0 -> Error `Closed
  | `Sent len -> Ok len

module Read = struct
  type t = read_file

  let rec read t ~buf =
    match Gluon.Syscall.read t.fd buf ~pos:0 ~len:(Io.Bytes.length buf) with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry -> syscall "File.read" `r t.fd @@ fun _ -> read t ~buf
    | `Read len -> Ok len

  let rec read_vectored t ~bufs =
    match Gluon.Syscall.readv t.fd bufs with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry ->
        syscall "File.read_vectored" `r t.fd @@ fun _ -> read_vectored t ~bufs
    | `Read len -> Ok len
end

let to_reader t = Io.Reader.of_read_src (module Read) t

module Write = struct
  type t = write_file

  let size t = (stat t).st_size

  let rec write_owned_vectored t ~bufs =
    match Gluon.Syscall.writev t.fd bufs with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry ->
        syscall "File.write_owned_vectored" `r t.fd @@ fun _ ->
        write_owned_vectored t ~bufs
    | `Wrote len -> Ok len

  let write t ~buf =
    let bufs = Io.Iovec.of_bytes buf in
    write_owned_vectored t ~bufs

  let flush _t = Ok ()
end

let to_writer t = Io.Writer.of_write_src (module Write) t
