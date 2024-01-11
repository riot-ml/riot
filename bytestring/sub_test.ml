open Bytestring

let view str off len = to_string (sub ~off ~len str)

let () =
  (* Basic ASCII *)
  let hello_world =
    of_string "Hello" ^ (of_string " " ^ of_string "W") ^ of_string "orld"
  in
  assert (view hello_world 0 10 = "Hello Worl");

  (* UTF-8 Multibyte Characters *)
  let str = of_string "こんに" ^ (of_string "ちは" ^ empty) ^ of_string "世界" in
  assert (view str 12 9 = "は世界");

  let packet =
    of_string "\001" ^ (of_string "" ^ of_string "\000") ^ of_string "\003\002"
  in
  assert (view packet 1 1 = "\000");
  assert (view packet 2 1 = "\003");
  assert (view packet 2 2 = "\003\002");
  assert (view packet 1 3 = "\000\003\002");

  (* Empty String *)
  assert (view (of_string "") 0 0 = "");

  print_endline "sub_test: OK"
