open Binstring
open Extract

let () =
  assert (get_byte (of_string "Hello World") 0 = 'H');
  assert (get_byte (of_string "\001\002\003\004") 2 = '\003');
  assert (get_byte (of_string "\001\002\003\004") 0 = '\001');
  print_endline "get_byte_test: OK"
