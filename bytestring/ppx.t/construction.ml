let () =

  (* test: a bytestring constructed with a single value that is all bytes is
     equal to itself
  *)
  let all = Bytestring.empty in
  let len = Bytestring.empty in
  let body = Bytestring.empty in
  let _str = {%b| all |} in
  let _str = {%b| all::bytes |} in
  let _str = {%b| all::utf8 |} in
  let _str = {%b| all::bytes |} in
  let _str = {%b| all::bytes(10) |} in

  let all = 2112 in
  let _str = {%b| all::8 |} in

  (* note: to use a byte length out of a bytestring, use `bytes(n)` *)
  let _str = {%b| len::bytes(1), body::bytes(10) |} in
  let _str = {%b| all::1024 |} in

  let all = {%b| len::bytes(123) |} in
  let one = 2112 in
  let _str = {%b| one::8, all::bytes |} in

  let fin = 0 in
  let comp = 0 in
  let mask = 2112 in
  let payload = {%b| "this is my data"::utf8 |} in
  let rest = {%b| "here's the rest"::bytes |} in
  let len = 9000 in
  let _str =
    {%b|
      fin::1, comp::1, 0::2, 1::4, 0::1, 127::7,
      len::bits(8*8), mask::32, payload::bytes(len), rest,
    |}
  in

  ()
