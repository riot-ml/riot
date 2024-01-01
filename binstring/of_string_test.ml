open Binstring

let () =
  (* Basic ASCII *)
  assert (to_string (of_string "Hello World") = "Hello World");

  (* UTF-8 Multibyte Characters *)
  assert (to_string (of_string "こんにちは世界") = "こんにちは世界");
  assert (to_string (of_string "🌍🌎🌏") = "🌍🌎🌏");

  (* Empty String *)
  assert (to_string (of_string "") = "");

  (* Strings with White Spaces *)
  assert (to_string (of_string " \t\n") = " \t\n");

  (* Strings with Special Characters *)
  assert (to_string (of_string "!@#$%^&*()") = "!@#$%^&*()");

  (* Malformed UTF-8 Sequences - Assuming they are skipped or replaced *)
  (match of_string "\xFF" with
  | exception Malformed _ -> ()
  | _ -> assert false);

  (* Very Long Strings *)
  let long_string = String.make 1000000 'a' in
  assert (to_string (of_string long_string) = long_string);

  (* Unicode Edge Cases *)
  assert (to_string (of_string "\u{1F600}\u{1F4A9}") = "\u{1F600}\u{1F4A9}");

  (* Add more tests as needed *)
  print_endline "of_string_test: OK"
