open Gluon
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

module Read = struct
  type t = read_file

  let rec read t ?timeout buf =
    match File.read t.fd buf ~pos:0 ~len:(Io.Bytes.length buf) with
    | Ok n -> Ok n
    | Error `Would_block ->
        syscall ?timeout "File.read" Interest.readable (File.to_source t.fd)
        @@ fun _ -> read t ?timeout buf
    | Error err -> Error err

  let rec read_vectored t bufs =
    match File.read_vectored t.fd bufs with
    | Ok n -> Ok n
    | Error `Would_block ->
        syscall "File.read_vectored" Interest.readable (File.to_source t.fd)
        @@ fun _ -> read_vectored t bufs
    | Error err -> Error err
end

let to_reader t = Io.Reader.of_read_src (module Read) t

module Write = struct
  type t = write_file

  let size t = (stat t).st_size

  let rec write_owned_vectored t ~bufs =
    match File.write_vectored t.fd bufs with
    | Ok n -> Ok n
    | Error `Would_block ->
        syscall "File.write_vectored" Interest.writable (File.to_source t.fd)
        @@ fun _ -> write_owned_vectored t ~bufs
    | Error err -> Error err

  let write t ~buf =
    let bufs = Io.Iovec.from_string buf in
    write_owned_vectored t ~bufs

  let flush _t = Ok ()
end

let to_writer t = Io.Writer.of_write_src (module Write) t
