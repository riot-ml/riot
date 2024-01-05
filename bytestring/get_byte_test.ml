open Bytestring
open Extract

let () =
  let binstr =
    of_string "Hello" ^ of_string " " ^ of_string "W" ^ of_string "orld"
  in
  assert (get_byte binstr 0 = 'H');
  assert (get_byte binstr 6 = 'W');

  let binstr = of_string "\001\002" ^ of_string "" ^ of_string "\003" in
  assert (get_byte binstr 2 = '\003');
  assert (get_byte binstr 0 = '\001');
  print_endline "get_byte_test: OK"
