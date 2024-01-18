open Runtime_events
open User

type tag +=
  | Scheduler_loop
  | Step_process
  | Handle_run_proc
  | Handle_exit_proc
  | Handle_wait_proc
  | Handle_syscall
  | Handle_receive
  | Poll_io

let span name event =
  let span = register name event Type.span in
  let start () = write span Type.Begin in
  let finish () = write span Type.End in
  (start, finish)

let scheduler_loop_span = register "riot.scheduler.loop" Scheduler_loop Type.span
let scheduler_loop_begin () = write scheduler_loop_span Type.Begin
let scheduler_loop_end () = write scheduler_loop_span Type.End

let handle_run_proc_start, handle_run_proc_finish =
  span "riot.scheduler.handle_run_proc" Handle_run_proc

let start, finish = span "riot.scheduler.handle_exit_proc" Handle_exit_proc

let handle_exit_proc_span fn =
  start ();
  let value = fn () in
  finish ();
  value

let start, finish = span "riot.scheduler.handle_wait_proc" Handle_wait_proc

let handle_wait_proc_span fn =
  start ();
  let value = fn () in
  finish ();
  value

let start, finish = span "riot.scheduler.handle_syscall" Handle_syscall

let handle_syscall_span fn =
  start ();
  let value = fn () in
  finish ();
  value

let start, finish = span "riot.scheduler.handle_receive" Handle_receive

let handle_receive_span fn =
  start ();
  let value = fn () in
  finish ();
  value

let start, finish = span "riot.io.poll_io_span" Poll_io

let poll_io_span fn =
  start ();
  let value = fn () in
  finish ();
  value
