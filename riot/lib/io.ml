module Low_level = Runtime.Net.Io

type read = Low_level.read
type write = Low_level.write

let read = Low_level.read
let write = Low_level.write

let rec single_read fd ~buf =
  match Low_level.readv fd [| buf |] with
| `Abort err -> Error (`Unix_error err)
| `Read read -> Ok read
| `Retry -> Runtime.syscall "single_read" `r fd @@ single_read ~buf

let rec single_write fd ~data =
  match Low_level.writev fd [| data |] with
| `Abort err -> Error (`Unix_error err)
| `Read read -> Ok read
| `Retry -> Runtime.syscall "single_write" `w fd @@ single_write ~data

let await_readable fd fn = Runtime.syscall "custom" `r fd fn
let await_writeable fd fn = Runtime.syscall "custom" `w fd fn
let await fd mode fn = Runtime.syscall "custom" mode fd fn

let copy _fd _cs =  ()
