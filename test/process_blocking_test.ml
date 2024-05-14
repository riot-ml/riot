[@@@warning "-8"]

open Riot

type Message.t += AnswerToAllTheWorldsProblems of int
type Message.t += CountdownFinished

let factorial n =
  let rec aux n acc =
    Logger.info (fun f -> f "Factorial %d" n);
    match n with 1 -> acc | x -> aux (n - 1) (acc * x)
  in
  aux n 1

let busy_worker recipient_pid () =
  let number = factorial 5 in
  send recipient_pid (AnswerToAllTheWorldsProblems number)

let rec countdown_worker recipient_pid n =
  Logger.info (fun f -> f "Countdown loop n = %d" n);

  if n = 0 then send recipient_pid CountdownFinished
  else (
    yield ();
    countdown_worker recipient_pid (n - 1))

let rec wait_for_answer () =
  print_endline "RECV";
  match receive_any () with
  | AnswerToAllTheWorldsProblems n ->
      Printf.printf
        "Got the answer!\n\
        \ The answer to all the worlds problems has been calculated to be %d\n"
        n
  | _ -> wait_for_answer ()

let () =
    print_endline "yooo";
  Riot.run ~workers:0 @@ fun () ->
  Runtime.set_log_level (Some Trace);

  let main_pid = self () in

  let pid_waiting = spawn wait_for_answer in
  let _factorial_answer_pid = spawn_blocking (busy_worker pid_waiting) in
  let _countdown_pid = spawn (fun () -> countdown_worker main_pid 1000) in
  wait_pids [ pid_waiting ];
  flush_all ();
  shutdown ()
