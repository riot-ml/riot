open Bytestring

let () =
  (* Basic ASCII *)
  let hello_world =
    of_string "Hello" ^ (of_string " " ^ of_string "W") ^ of_string "orld"
  in
  assert (to_string hello_world = "Hello World");

  (* UTF-8 Multibyte Characters *)
  let str = of_string "こんに" ^ (of_string "ちは" ^ empty) ^ of_string "世界" in
  assert (to_string str = "こんにちは世界");
  let str =
    of_string "🌍" ^ (of_string "" ^ empty ^ of_string "🌎") ^ of_string "🌏"
  in
  assert (to_string str = "🌍🌎🌏");

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
