let () =
  let str = {%b| |} in
  let compute_bits () = 8 * 8 in
  let my_guard () = false in
  let do_something _ _ = 0 in
  let _ =
    match%b str with
    | {| fin::1,
         _comp::1,
         0::2,
         1::4,
         0::1,
         127::7,
         len::bits(compute_bits ()),
         _mask::32,
         payload::bytes(len),
         _rest,
        |}
      when my_guard () ->
        do_something fin payload
    | {| fin::1,
         comp::1,
         0::2,
         1::4,
         0::1,
         127::7,
         len::bits(compute_bits ()),
         _mask::32,
         _payload::bytes(len),
         _rest,
        |}
      ->
        fin + comp
    | {| fin::1, comp::1, 0::2, 1::4, 0::1, 127::7, |} -> fin + comp
    | {| fin::1, comp::1 |} -> fin + comp
    | {| rest |} -> Bytestring.length rest
  in

  let _ =
    match%b str with
    | {| fin::1,
       compressed::1,
       _rsv::2,
       _opcode::4,
       0::1,
       126::7,
       length::64,
       _mask::32,
       _payload::bytes(length * 8),
       _rest::bytes|}
      when compressed = 0 || length > 0 ->
        fin
    | {| _fin::1,
       compressed::1,
       _rsv::2,
       _opcode::4,
       0::1,
       126::7,
       length::16,
       _mask::32,
       _payload::bytes(length * 8),
       rest::bytes|}
      when rest != Bytestring.empty ->
        compressed
    | {| fin::1,
       _compressed::1,
       rsv::2,
       _opcode::4,
       0::1,
       length::7,
       mask::32,
       _payload::bytes(length * 8),
       _rest::bytes |}
      when length <= 125 && (fin == 0 || length <= mask) ->
        rsv
    | {| data::bytes  |} -> Bytestring.length data
  in

  ()
