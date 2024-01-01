open Runtime

type t = Runtime.Process.t
type priority = Runtime.Process.priority = High | Normal | Low

type process_flag = Runtime.Process.process_flag =
  | Trap_exit of bool
  | Priority of priority

type exit_reason = Runtime.Process.exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

module Messages = Runtime.Process.Messages

let pp = Runtime.Process.pp
let where_is = Runtime.where_is

let rec await_name name =
  match where_is name with
  | Some pid -> pid
  | None ->
      yield ();
      await_name name
