open Riot_runtime.Import
module P = Riot_runtime.Core.Process

open Logger.Make (struct
  let namespace = [ "riot"; "process" ]
end)

type t = P.t
type priority = P.priority = High | Normal | Low

type process_flag = P.process_flag =
  | Trap_exit of bool
  | Priority of priority
  | IsBlockingProc of bool

let pp_flag fmt t =
  match t with
  | Trap_exit b -> Format.fprintf fmt "trap_exit <- %b" b
  | Priority p -> Format.fprintf fmt "priority <- %s" (P.priority_to_string p)
  | _ -> failwith "TODO"

type exit_reason = P.exit_reason =
  | Normal
  | Exit_signal
  | Bad_link
  | Link_down of Pid.t
  | Exception of exn

let pp_reason = P.pp_reason

module Messages = P.Messages

let pp = P.pp
let where_is = where_is
let sid = P.sid

let rec await_name name =
  match where_is name with
  | Some pid -> pid
  | None ->
      yield ();
      await_name name

let flag flag =
  trace (fun f -> f "%a updated flag: %a" Pid.pp (self ()) pp_flag flag);
  process_flag flag

let monitor pid =
  trace (fun f -> f "%a is now monitoring %a" Pid.pp (self ()) Pid.pp pid);
  monitor pid

let demonitor pid =
  trace (fun f -> f "%a is no longer monitoring %a" Pid.pp (self ()) Pid.pp pid);
  demonitor pid

let is_alive pid = is_process_alive pid
