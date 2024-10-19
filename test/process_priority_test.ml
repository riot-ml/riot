open Riot

type Message.t += A | B | C

let rec loop pid msg n =
  if n = 0 then send pid msg
  else (
    yield ();
    loop pid msg (n - 1))

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let this = self () in

  let _ =
    spawn (fun () ->
        process_flag (Priority Low);
        loop this A 100)
  in
  let _ =
    spawn (fun () ->
        process_flag (Priority Normal);
        loop this B 100)
  in
  let _ =
    spawn (fun () ->
        process_flag (Priority High);
        loop this C 100)
  in

  let m1 = receive_any ~after:50_000L () in
  let m2 = receive_any ~after:50_000L () in
  let m3 = receive_any ~after:50_000L () in

  match (m1, m2, m3) with
  | C, B, A ->
      Logger.info (fun f -> f "process_priority_test: OK");

      shutdown ()
  | m1, m2, m3 ->
      Logger.error (fun f ->
          f "process_priority_test: messages arrived out of order?\n%S\n%S\n%S"
            (Marshal.to_string m1 []) (Marshal.to_string m2 [])
            (Marshal.to_string m3 []));

      Stdlib.exit 1

(* NOTE(@leostera): this test NEEDS to run on just one scheduler, so we spin up
   no additional workers. The reason is that if we do, then other schedulers
   may pick up lower priority process earlier, and so the message order will be
   different.

   That behavior _is expected_.
*)
let () = Riot.run ~config:(Config.make ~workers:0 ()) @@ main
