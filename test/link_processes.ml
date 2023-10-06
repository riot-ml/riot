(**
   NOTE(leostera): this tests that if you link a process to another, when
   that second process finished the first one is also terminated.

   If you look at the erlang counterpart for this test, you'll see that we
   can simply hold on a `receive` there and the processes are executed as
   we expect. I know there's a race condition there as well, but i can't seem to trigger it very reliably. Most of the time, the second process will link faster than the first process will die.

   In our case however, since we don't have the kind of preemptive scheduling that Erlang has, the execution order is expected to be morel ike:

     1. spawn Pid1
     2. spawn Pid2
     3. send Exit message
     4. Pid1 loops + yield
     5. Pid2 link + loops + yield
     {repeat until i = 0}
     7. Pid1 receives and ignores message, terminating
     8. Pid2 is terminated
     9. we eventually fulfill `wait_pids` and shutdown

*)
open Riot

type Message.t += Exit

let rec loop i =
  if i > 0 then (
    yield ();
    loop (i - 1))
  else receive () |> ignore

let rec wait_pids pids =
  match pids with
  | [] -> ()
  | pid :: tail -> wait_pids (if is_process_alive pid then pids else tail)

let main () =
  let pid1 = spawn (fun () -> loop 100) in

  let pid2 =
    spawn (fun () ->
        link pid1;
        loop 0)
  in

  send pid1 Exit;

  wait_pids [ pid1; pid2 ];
  Logs.log (fun f -> f "linked processes terminated");
  shutdown ()

let () =
  Logs.set_log_level (Some Info);
  Riot.run @@ main
