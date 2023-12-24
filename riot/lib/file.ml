type 'kind file = Fd.t
type read_file = [ `r ] file
type write_file = [ `w ] file
type rw_file = [ `w | `r ] file

let fd t = t
let base_permissions = 0o640

let do_open path flags =
  let raw_fd = Unix.openfile path flags base_permissions in
  Fd.make raw_fd

let open_read path = do_open path Unix.[ O_RDONLY ]
let open_write path = do_open path Unix.[ O_WRONLY; O_CREAT ]
let close t = Fd.close t
let remove path = Unix.unlink path

module Read = Io.Reader.Make (struct
  type t = read_file

  let rec read t ~buf =
    match Runtime.Net.Io.readv t [| Io.Buffer.as_cstruct buf |] with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry -> Runtime.syscall "File.read" `r t @@ read ~buf
    | `Read len -> Ok len
end)

let to_reader t = Io.Reader.of_read_src (module Read) t

module Write = Io.Writer.Make (struct
  type t = write_file

  let rec write t ~data =
    match Runtime.Net.Io.writev t [| Io.Buffer.as_cstruct data |] with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry -> Runtime.syscall "File.write" `r t @@ write ~data
    | `Wrote len -> Ok len

  let flush _t = Ok ()
end)

let to_writer t = Io.Writer.of_write_src (module Write) t
