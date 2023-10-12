[@@@warning "-8"]

open Riot
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

let rec loop () =
  yield ();
  loop ()

let main () =
  let (Ok ()) = Logger.start () in

  (* spin up and wait for 1 second before terminating *)
  let pid1 =
    spawn (fun () ->
        sleep 1.;
        loop ())
  in

  (* spin up, link to pid1, and then loop infinitely *)
  let pid2 =
    spawn (fun () ->
        link pid1;
        loop ())
  in

  (* once we send this exit signal to pid1, and it dies, it should take pid2 down with it *)
  exit pid1 Normal;

  (* so we'll wait for both pids to be dead *)
  wait_pids [ pid1; pid2 ];

  Logger.info (fun f -> f "linked processes terminated");
  sleep 0.001;
  shutdown ()

let () =
  Logger.set_log_level (Some Info);
  Riot.run @@ main
