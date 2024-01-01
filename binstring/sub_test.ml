open Binstring

let view str off len = (to_string (of_string str |> sub ~off ~len))

let () =
  (* Basic ASCII *)
  assert (view "Hello World" 0 10 = "Hello Worl");

  (* UTF-8 Multibyte Characters *)
  assert (view "こんにちは世界" 12 21 = "は世界");
  assert (view "\000\000\003\002" 2 3 = "\003");

  (* Empty String *)
  assert (view "" 0 0 = "");

  print_endline "sub_test: OK"
