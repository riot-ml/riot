[@@@warning "-8"]

open Riot

type Message.t += AnswerToAllTheWorldsProblems of int

(* Factorial is too fast so just a little function that eats some more CPU time*)
let rec block_longer n = if n == 0 then () else block_longer (n - 1)

let factorial n =
  let rec aux n acc =
    Logger.info (fun f -> f "Factorial %d" n);
    block_longer 100000;
    match n with 1 -> acc | x -> aux (n - 1) (acc * x)
  in
  aux n 1

let busy_worker recipient_pid () =
  let number = factorial 30 in
  send recipient_pid (AnswerToAllTheWorldsProblems number)

let rec countdown_worker n =
  Logger.info (fun f -> f "Countdown loop n = %d" n);

  if n = 0 then ()
  else (
    yield ();
    countdown_worker (n - 1))

let rec wait_for_answer () =
  match receive_any () with
  | AnswerToAllTheWorldsProblems n ->
      Printf.printf
        "Got the answer!\n\
        \ The answer to all the worlds problems has been calculated to be %d\n"
        n
  | _ -> wait_for_answer ()

let () =
  Runtime.set_log_level (Some Trace);
  print_endline "Test spawn_blocking";
  Riot.run ~workers:0 @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Debug);

  (* let main_pid = self () in *)
  let pid_waiting = spawn wait_for_answer in

  let _countdown_pid = spawn (fun () -> countdown_worker 100) in
  let _factorial_answer_pid = spawn_blocking (busy_worker pid_waiting) in
  wait_pids [ pid_waiting ];
  flush_all ();
  shutdown ()
