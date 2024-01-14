include Io

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "io" ]
end)

(* let await_readable fd fn = syscall "custom" `r fd fn *)
(* let await_writeable fd fn = syscall "custom" `w fd fn *)
(* let await fd mode fn = syscall "custom" mode fd fn *)
