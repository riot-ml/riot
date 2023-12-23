type 'kind file = Fd.t
type read_file = [ `r ] file
type write_file = [ `w ] file
type rw_file = [ `w | `r ] file

let fd t = t

let base_permissions = 0o640

let do_open path flags = 
  let raw_fd = Unix.openfile path flags base_permissions in
  Fd.make raw_fd

let open_read path = do_open path Unix.[O_RDONLY]

let open_write path = do_open path Unix.[O_WRONLY;O_CREAT]

let close t = Fd.close t

let remove path = Unix.unlink path
