  $ dune build
  File "error_empty_comma.ml", line 1, characters 17-35:
  1 | let () = assert ({%bytestring| , |} = Bytestring.empty)
                       ^^^^^^^^^^^^^^^^^^
  Error: The bytestring syntax supports trailing commas, but a single comma is
         not a valid bytestring pattern.
         
  File "error_invalid_number_bytes.ml", line 1, characters 8-36:
  1 | let _ = {%bytestring| 2112::bytes |}
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: We found an invalid size: bytes for value 2112
         
         Valid sizes are for literal numbers are : bytes(expr), bits(expr), a
         number (like 3, 7, or 2112).
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_number_size.ml", line 1, characters 9-37:
  1 | let () = {%bytestring| 2112::bytes |}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: We found an invalid size: bytes for value 2112
         
         Valid sizes are for literal numbers are : bytes(expr), bits(expr), a
         number (like 3, 7, or 2112).
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_number_size_utf8.ml", line 1, characters 9-40:
  1 | let () = {%bytestring| 2112::utf8(10) |}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: We found an invalid size: utf8(10) for value 2112
         
         Valid sizes are for literal numbers are : bytes(expr), bits(expr), a
         number (like 3, 7, or 2112).
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_string_size.ml", line 1, characters 8-37:
  1 | let _ = {%bytestring| "rush"::2112 |}
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: We found an invalid size: 2112 for value "rush"
         
         Valid sizes are for string literals are: bytes, bytes(expr), utf8, and
         utf8(expr).
         
         For example:
         
           "rush"::bytes      – use all of the "rush" string
           "rush"::bytes(10)  – use "rush" as a 10-byte string
           "rush"::utf8       – use "rush" as a valid UTF-8 string
           "rush"::utf8(10)   – use "rush" as a 10-grapheme utf-8 string
         
         
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable opcode.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable mask.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable payload.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable rest.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable mask.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable payload.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable payload.
  
  File "_none_", line 1:
  Error (warning 26 [unused-var]): unused variable rest.
  [1]
  $ dune describe pp ./empty.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = [("library-name", "test")]
    }]
  let () = assert (Bytestring.empty = Bytestring.empty)
  $ dune describe pp ./construction.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = [("library-name", "test")]
    }]
  let () =
    let all = Bytestring.empty in
    let len = Bytestring.empty in
    let body = Bytestring.empty in
    let _str = all in
    let _str = all in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_utf8 _trns all;
      Bytestring.Transient.commit _trns in
    let _str = all in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:10 all;
      Bytestring.Transient.commit _trns in
    let all = 2112 in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:8 all;
      Bytestring.Transient.commit _trns in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:1 len;
      Bytestring.Transient.add_string _trns ~size:10 body;
      Bytestring.Transient.commit _trns in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:1024 all;
      Bytestring.Transient.commit _trns in
    let all =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:123 len;
      Bytestring.Transient.commit _trns in
    let one = 2112 in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:8 one;
      Bytestring.Transient.add_string _trns all;
      Bytestring.Transient.commit _trns in
    let fin = 0 in
    let comp = 0 in
    let mask = 2112 in
    let payload =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_utf8 _trns "this is my data";
      Bytestring.Transient.commit _trns in
    let rest =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_string _trns "here's the rest";
      Bytestring.Transient.commit _trns in
    let len = 9000 in
    let _str =
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:1 fin;
      Bytestring.Transient.add_bits _trns ~size:1 comp;
      Bytestring.Transient.add_literal_int _trns ~size:2 0;
      Bytestring.Transient.add_literal_int _trns ~size:4 1;
      Bytestring.Transient.add_literal_int _trns ~size:1 0;
      Bytestring.Transient.add_literal_int _trns ~size:7 127;
      Bytestring.Transient.add_bits _trns ~size:(8 * 8) len;
      Bytestring.Transient.add_bits _trns ~size:32 mask;
      Bytestring.Transient.add_string _trns ~size:len payload;
      Bytestring.Transient.add_string _trns rest;
      Bytestring.Transient.commit _trns in
    ()
  $ dune describe pp ./matching.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = [("library-name", "test")]
    }]
  let () =
    let str = Bytestring.empty in
    let compute_bits () = 8 * 8 in
    let my_guard () = false in
    let do_something _ _ = 0 in
    let _ =
      (fun _data_src ->
         try
           let _data_src = Bytestring.to_iter _data_src in
           let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
           try
             try
               let _comp = Bytestring.Iter.next_bits ~size:1 _data_src in
               Bytestring.Iter.expect_literal_int _data_src ~size:2 0;
               Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
               Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
               Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
               (let len =
                  Bytestring.Iter.next_bits ~size:(compute_bits ()) _data_src in
                let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                let payload = Bytestring.Iter.next_bytes ~size:len _data_src in
                let _rest = Bytestring.Iter.rest _data_src in
                Bytestring.Iter.expect_empty _data_src;
                if my_guard ()
                then do_something fin payload
                else raise Bytestring.Guard_mismatch)
             with
             | Bytestring.No_match ->
                 (try
                    let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
                    Bytestring.Iter.expect_literal_int _data_src ~size:2 0;
                    Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
                    Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
                    Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
                    (let len =
                       Bytestring.Iter.next_bits ~size:(compute_bits ())
                         _data_src in
                     let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                     let _payload =
                       Bytestring.Iter.next_bytes ~size:len _data_src in
                     let _rest = Bytestring.Iter.rest _data_src in
                     Bytestring.Iter.expect_empty _data_src; fin + comp)
                  with | Bytestring.No_match -> raise Bytestring.No_match)
           with
           | Bytestring.No_match ->
               (try
                  let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
                  Bytestring.Iter.expect_literal_int _data_src ~size:2 0;
                  Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
                  Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
                  Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
                  Bytestring.Iter.expect_empty _data_src;
                  fin + comp
                with
                | Bytestring.No_match ->
                    (try
                       let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
                       Bytestring.Iter.expect_empty _data_src; fin + comp
                     with | Bytestring.No_match -> raise Bytestring.No_match))
         with
         | Bytestring.No_match ->
             (try let rest = _data_src in Bytestring.length rest
              with | Bytestring.No_match -> raise Bytestring.No_match)) str in
    let _ =
      (fun _data_src ->
         try
           let _data_src = Bytestring.to_iter _data_src in
           let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
           let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
           let rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
           let opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
           Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
           (try
              Bytestring.Iter.expect_literal_int _data_src ~size:7 126;
              (try
                 let length = Bytestring.Iter.next_bits ~size:64 _data_src in
                 let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                 let payload =
                   Bytestring.Iter.next_bytes ~size:(length * 8) _data_src in
                 let rest = Bytestring.Iter.rest _data_src in
                 Bytestring.Iter.expect_empty _data_src;
                 if (compressed = 0) || (length > 0)
                 then fin
                 else raise Bytestring.Guard_mismatch
               with
               | Bytestring.No_match ->
                   (try
                      let length = Bytestring.Iter.next_bits ~size:16 _data_src in
                      let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                      let payload =
                        Bytestring.Iter.next_bytes ~size:(length * 8) _data_src in
                      let rest = Bytestring.Iter.rest _data_src in
                      Bytestring.Iter.expect_empty _data_src;
                      if rest != Bytestring.empty
                      then compressed
                      else raise Bytestring.Guard_mismatch
                    with | Bytestring.No_match -> raise Bytestring.No_match))
            with
            | Bytestring.No_match ->
                (try
                   let length = Bytestring.Iter.next_bits ~size:7 _data_src in
                   let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                   let payload =
                     Bytestring.Iter.next_bytes ~size:(length * 8) _data_src in
                   let rest = Bytestring.Iter.rest _data_src in
                   Bytestring.Iter.expect_empty _data_src;
                   if (length <= 125) && ((fin == 0) || (length <= mask))
                   then rsv
                   else raise Bytestring.Guard_mismatch
                 with | Bytestring.No_match -> raise Bytestring.No_match))
         with
         | Bytestring.No_match ->
             (try let data = _data_src in Bytestring.length data
              with | Bytestring.No_match -> raise Bytestring.No_match)) str in
    ()
