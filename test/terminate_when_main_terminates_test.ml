let () =
  (* start the runtime with a function that will immediately return *)
  Riot.run @@ fun () ->
  ();
(* print that everything is OK *)
  print_string "termination_test: OK"

