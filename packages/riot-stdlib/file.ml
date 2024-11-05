open Gluon
open Global

type 'kind file = { fd : Fd.t; path : string }
type read_file = [ `r ] file
type write_file = [ `w ] file
type rw_file = [ `w | `r ] file

let fd t = t.fd
let base_permissions = 0o640

let do_open ?(permissions = base_permissions) path flags =
  let raw_fd = Unix.openfile path flags permissions in
  { fd = Fd.make raw_fd; path }

let open_read ?permissions path = do_open ?permissions path Unix.[ O_RDONLY ]

let open_write ?permissions path =
  do_open ?permissions path Unix.[ O_WRONLY; O_CREAT ]

let close t = Fd.close t.fd
let remove path = Unix.unlink path
let seek t ~off = Fd.seek t.fd off Unix.SEEK_SET
let stat path = Unix.stat path

let exists path =
  match Unix.stat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | (exception _) | _ -> true

module Read = struct
  type t = read_file
  type error = [ `Gluon of Gluon.error ]

  let rec read t ?timeout buf =
    match
      File.read t.fd buf ~pos:0 ~len:(Rio.Bytes.length buf)
      |> Result.map_error (fun e -> `Gluon e)
    with
    | Ok n -> Ok n
    | Error (`Gluon (Syscall_would_block _)) ->
        syscall ?timeout "File.read" Interest.readable (File.to_source t.fd)
        @@ fun _ -> read t ?timeout buf
    | Error err -> Error err

  let rec read_vectored t bufs =
    match
      File.read_vectored t.fd bufs |> Result.map_error (fun e -> `Gluon e)
    with
    | Ok n -> Ok n
    | Error (`Gluon (Syscall_would_block _)) ->
        syscall "File.read_vectored" Interest.readable (File.to_source t.fd)
        @@ fun _ -> read_vectored t bufs
    | Error err -> Error err
end

let to_reader t = Rio.Reader.of_read_src (module Read) t

module Write = struct
  type t = write_file
  type error = [ `Gluon of Gluon.error ]

  let size t = (stat t).st_size

  let rec write_owned_vectored t ~bufs =
    match
      File.write_vectored t.fd bufs |> Result.map_error (fun e -> `Gluon e)
    with
    | Ok n -> Ok n
    | Error (`Gluon (Syscall_would_block _)) ->
        syscall "File.write_vectored" Interest.writable (File.to_source t.fd)
        @@ fun _ -> write_owned_vectored t ~bufs
    | Error err -> Error err

  let write t ~buf =
    let bufs = Rio.Iovec.from_string buf in
    write_owned_vectored t ~bufs

  let flush _t = Ok ()
end

let to_writer t = Rio.Writer.of_write_src (module Write) t
