let () =
  (* test: empty pattern is just an empty bytestring *)
  assert ({%bytestring| |} = Bytestring.empty);

  (* test: a bytestring constructed with a single value that is all bytes is
     equal to itself
  *)
  let all = Bytestring.empty in
  assert ({%bytestring| all::bytes |} = all);

  (* test: a bytestring constructed with a single value without a size annotation
     is an error
  *)
  let all = Bytestring.empty in
  assert ({%bytestring| all |} = all);
  ()
