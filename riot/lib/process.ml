open Runtime
module P = Runtime.Core.Process

type t = P.t
type priority = P.priority = High | Normal | Low
type process_flag = P.process_flag = Trap_exit of bool | Priority of priority

type exit_reason = P.exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

let pp_reason = P.pp_reason

module Messages = P.Messages

let pp = P.pp
let where_is = Import.where_is

let rec await_name name =
  match where_is name with
  | Some pid -> pid
  | None ->
      yield ();
      await_name name

let demonitor = Import.demonitor
