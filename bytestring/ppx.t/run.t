  $ dune build
  File "error_empty_comma.ml", line 1, characters 17-26:
  1 | let () = assert ({%b| , |} = Bytestring.empty)
                       ^^^^^^^^^
  Error: Invalid bytestring pattern
         
         A bytestring pattern must have zero, one, or more fields separated by
         commas.
         
         Trailing commas are supported, but a single comma is not a valid
         bytestring pattern.
             
         
  File "error_invalid_number_bytes.ml", line 1, characters 8-27:
  1 | let _ = {%b| 2112::bytes |}
              ^^^^^^^^^^^^^^^^^^^
  Error: Invalid size "bytes" for value 2112
         
         Valid sizes for number literals are:
         
           bytes(expr) - use up to `expr` bytes
           bits(expr)  - use up to `expr` bits
           <number>    - use exactly `<number>` bits
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_number_size.ml", line 1, characters 9-28:
  1 | let () = {%b| 2112::bytes |}
               ^^^^^^^^^^^^^^^^^^^
  Error: Invalid size "bytes" for value 2112
         
         Valid sizes for number literals are:
         
           bytes(expr) - use up to `expr` bytes
           bits(expr)  - use up to `expr` bits
           <number>    - use exactly `<number>` bits
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_number_size_utf8.ml", line 1, characters 9-31:
  1 | let () = {%b| 2112::utf8(10) |}
               ^^^^^^^^^^^^^^^^^^^^^^
  Error: Invalid size "utf8(10)" for value 2112
         
         Valid sizes for number literals are:
         
           bytes(expr) - use up to `expr` bytes
           bits(expr)  - use up to `expr` bits
           <number>    - use exactly `<number>` bits
         
         For example:
         
           2112::bytes(10)  – use `2112` as a 10 byte integer
           2112::bits(len)  – use `2112` as a `len` bits integer
           2112::7          – use `2112` as a 7 bit integer
         
         
  File "error_invalid_string_size.ml", line 1, characters 8-28:
  1 | let _ = {%b| "rush"::2112 |}
              ^^^^^^^^^^^^^^^^^^^^
  Error: Invalid size "2112" for value "rush"
         
         Valid sizes for string literals are:
         
           bytes       - match on the entire string
           bytes(expr) - match `expr` bytes
           utf8        - match on 1 UTF-8 grapheme
           utf8(expr)  - match `expr` UTF-8 graphemes
         
         For example:
         
           "rush"::bytes      – use all of the "rush" string
           "rush"::bytes(10)  – use "rush" as a 10-byte string
           "rush"::utf8       – use "rush" as a valid UTF-8 string
           "rush"::utf8(10)   – use "rush" as a 10-grapheme utf-8 string
         
         
  File "match_error_invalid_expect_number_size.ml", lines 2-3, characters 2-44:
  2 | ..match%b {%b| |} with
  3 |   | {| 2112::utf8, 0::1, rest::rush |} -> ()
  Error: Invalid size "rush" for "rest"
         
         Valid sizes are:
         
           bytes       - match on the entire string
           bytes(expr) - match `expr` bytes
           utf8        - match on 1 UTF-8 grapheme
           utf8(expr)  - match `expr` UTF-8 graphemes
           bits(expr)  - use up to `expr` bits
           <number>    - use exactly `<number>` bits
         
         For example:
         
           hello::bytes       – use all of `hello` as a byte string
           hello::bytes(len)  – use `len` bytes from `hello`
           hello::bits(len)   – use len bits of `hello` (128 bytes)
           hello::utf8        – use 1 valid utf8 grapheme from `hello`
           hello::7           – use 8 bits of `hello`
         
         
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
           try
             let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
             let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
             let _rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
             let _opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
             Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
             Bytestring.Iter.expect_literal_int _data_src ~size:7 126;
             (let length = Bytestring.Iter.next_bits ~size:64 _data_src in
              let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
              let _payload =
                Bytestring.Iter.next_bytes ~size:(length * 8) _data_src in
              let _rest = Bytestring.Iter.rest _data_src in
              Bytestring.Iter.expect_empty _data_src;
              if (compressed = 0) || (length > 0)
              then fin
              else raise Bytestring.Guard_mismatch)
           with
           | Bytestring.No_match ->
               (try
                  let _fin = Bytestring.Iter.next_bits ~size:1 _data_src in
                  let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
                  let _rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
                  let _opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
                  Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
                  Bytestring.Iter.expect_literal_int _data_src ~size:7 126;
                  (let length = Bytestring.Iter.next_bits ~size:16 _data_src in
                   let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                   let _payload =
                     Bytestring.Iter.next_bytes ~size:(length * 8) _data_src in
                   let rest = Bytestring.Iter.rest _data_src in
                   Bytestring.Iter.expect_empty _data_src;
                   if rest != Bytestring.empty
                   then compressed
                   else raise Bytestring.Guard_mismatch)
                with
                | Bytestring.No_match ->
                    (try
                       let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
                       let _compressed =
                         Bytestring.Iter.next_bits ~size:1 _data_src in
                       let rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
                       let _opcode =
                         Bytestring.Iter.next_bits ~size:4 _data_src in
                       Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
                       (let length =
                          Bytestring.Iter.next_bits ~size:7 _data_src in
                        let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                        let _payload =
                          Bytestring.Iter.next_bytes ~size:(length * 8)
                            _data_src in
                        let _rest = Bytestring.Iter.rest _data_src in
                        Bytestring.Iter.expect_empty _data_src;
                        if (length <= 125) && ((fin == 0) || (length <= mask))
                        then rsv
                        else raise Bytestring.Guard_mismatch)
                     with | Bytestring.No_match -> raise Bytestring.No_match))
         with
         | Bytestring.No_match ->
             (try let data = _data_src in Bytestring.length data
              with | Bytestring.No_match -> raise Bytestring.No_match)) str in
    ()
