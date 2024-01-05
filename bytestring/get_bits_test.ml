open Bytestring
open Extract

let () =
  assert (get_bits (of_string "Hello World") 0 = 'H');
  assert (get_bits (of_string "\001\002\003\004") 2 = '\003');
  assert (get_bits (of_string "\001\002\003\004") 0 = '\001');
  print_endline "get_bits_test: OK"
