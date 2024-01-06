open Ppxlib
open Ast_helper
open Bytepattern

let id name =
  let longident = Location.{ loc = none; txt = Longident.parse name } in
  Exp.ident longident

let int n = Exp.constant (Const.int n)

let () =
  let test str expected =
    let actual = Lexer.read str in

    let actual_str = Format.asprintf "%a" Lexer.pp actual in
    let expect_str = Format.asprintf "%a" Lexer.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "lexer test %S OK\r\n%!" str
    else (
      Format.printf
        "Error tokens do not match, expected:\n\n%a\n\nbut found:\n\n%a\n\n"
        Lexer.pp expected Lexer.pp actual;
      assert false)
  in

  test "" [];
  test "all" [ IDENT "all" ];
  test "all::4" [ IDENT "all"; COLON_COLON; NUMBER 4 ];
  test "all::1024" [ IDENT "all"; COLON_COLON; NUMBER 1024 ];
  test "all::utf8" [ IDENT "all"; COLON_COLON; IDENT "utf8" ];

  test "2112::utf8" [ NUMBER 2112; COLON_COLON; IDENT "utf8" ];
  test {|"rush"::utf8|} [ STRING "rush"; COLON_COLON; IDENT "utf8" ];

  test "all::byte(10)"
    [ IDENT "all"; COLON_COLON; IDENT "byte"; EXPRESSION (int 10) ];

  test "all::byte(10),"
    [ IDENT "all"; COLON_COLON; IDENT "byte"; EXPRESSION (int 10); COMMA ];

  test "all::byte(10), rest::binary"
    [
      IDENT "all";
      COLON_COLON;
      IDENT "byte";
      EXPRESSION (int 10);
      COMMA;
      IDENT "rest";
      COLON_COLON;
      IDENT "binary";
    ];

  test
    {| fin::1, comp::1, _rsv::2, 1::4 , 0::1, 127::7 , len ::64

      ,
      mask::32
,payload::bytes(

  len

  ) 

      |}
    [
      IDENT "fin";
      COLON_COLON;
      NUMBER 1;
      COMMA;
      IDENT "comp";
      COLON_COLON;
      NUMBER 1;
      COMMA;
      IDENT "_rsv";
      COLON_COLON;
      NUMBER 2;
      COMMA;
      NUMBER 1;
      COLON_COLON;
      NUMBER 4;
      COMMA;
      NUMBER 0;
      COLON_COLON;
      NUMBER 1;
      COMMA;
      NUMBER 127;
      COLON_COLON;
      NUMBER 7;
      COMMA;
      IDENT "len";
      COLON_COLON;
      NUMBER 64;
      COMMA;
      IDENT "mask";
      COLON_COLON;
      NUMBER 32;
      COMMA;
      IDENT "payload";
      COLON_COLON;
      IDENT "bytes";
      EXPRESSION (id "len");
    ];
  ()

let () =
  let open Parser in
  let test str expected =
    let actual = Parser.parse str in

    let actual_str = Format.asprintf "%a" Parser.pp actual in
    let expect_str = Format.asprintf "%a" Parser.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "parser test %S OK\r\n%!" str
    else (
      Format.printf
        "Error parsetree does not match, expected:\n\n\
         %a\n\n\
         but found:\n\n\
         %a\n\n"
        Parser.pp expected Parser.pp actual;
      assert false)
  in

  test "" [];
  test "all" [ Bind { name = "all"; size = Rest } ];
  test "all::8" [ Bind { name = "all"; size = Fixed_bits 8 } ];
  test "all::1024" [ Bind { name = "all"; size = Fixed_bits 1024 } ];
  test "all::utf8" [ Bind { name = "all"; size = Utf8 } ];
  test "all::bytes" [ Bind { name = "all"; size = Rest } ];
  test "all::bytes(10)" [ Bind { name = "all"; size = Dynamic_bytes (int 10) } ];
  test "2112::utf8" [ Expect { value = Number 2112; size = Utf8 } ];
  test {|"rush"::utf8|} [ Expect { value = String "rush"; size = Utf8 } ];
  test "len::8, body::bytes(len)"
    [
      Bind { name = "len"; size = Fixed_bits 8 };
      Bind { name = "body"; size = Dynamic_bytes (id "len") };
    ];
  test "one::8, all::bytes"
    [
      Bind { name = "one"; size = Fixed_bits 8 };
      Bind { name = "all"; size = Rest };
    ];
  test
    {| fin::1, comp::1, _rsv::2, 1::4, 0::1, 127::7, len::bytes(8), mask::32, payload::bytes(len), rest |}
    [
      bind "fin" (Fixed_bits 1);
      bind "comp" (Fixed_bits 1);
      bind "_rsv" (Fixed_bits 2);
      expect (Number 1) (Fixed_bits 4);
      expect (Number 0) (Fixed_bits 1);
      expect (Number 127) (Fixed_bits 7);
      bind "len" (Dynamic_bytes (int 8));
      bind "mask" (Fixed_bits 32);
      bind "payload" (Dynamic_bytes (id "len"));
      bind "rest" Rest;
    ];
  ()

(**
   Lowered representation of the pattern language for Constructing matches
*)
let () =
  let open Construction_lower in
  let test str expected =
    let actual = parse str |> Construction_lower.lower in
    let actual_str = Format.asprintf "%a" Construction_lower.pp actual in
    let expect_str = Format.asprintf "%a" Construction_lower.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "cstr-low test %S OK\r\n%!" str
    else (
      Format.printf
        "Error %S – lowered repr doesn't match, expected:\n\n\
         %a\n\n\
         but found:\n\n\
         %a\n\n"
        str Construction_lower.pp expected Construction_lower.pp actual;
      assert false)
  in

  test "" [ Empty ];
  test "all" [ Bypass "all" ];
  test "all::8"
    [
      Create_transient "_trns";
      Add_next_fixed_bits { src = "all"; size = 8 };
      Commit_transient "_trns";
    ];
  test "all::1024"
    [
      Create_transient "_trns";
      Add_next_fixed_bits { src = "all"; size = 1024 };
      Commit_transient "_trns";
    ];
  test "all::utf8"
    [
      Create_transient "_trns";
      Add_next_utf8 { src = "all" };
      Commit_transient "_trns";
    ];
  test "all::bytes" [ Bypass "all" ];
  test "all::bytes(10)"
    [
      Create_transient "_trns";
      Add_next_dynamic_bytes { src = "all"; expr = int 10 };
      Commit_transient "_trns";
    ];
  test "2112::1"
    [
      Create_transient "_trns";
      Add_int_fixed_bits { value = 2112; size = 1 };
      Commit_transient "_trns";
    ];
  test "2112::bits(1234)"
    [
      Create_transient "_trns";
      Add_int_dynamic_bits { value = 2112; expr = int 1234 };
      Commit_transient "_trns";
    ];
  test "2112::bytes(1234)"
    [
      Create_transient "_trns";
      Add_int_dynamic_bytes { value = 2112; expr = int 1234 };
      Commit_transient "_trns";
    ];
  test {|"rush"::utf8|}
    [
      Create_transient "_trns";
      Add_string_utf8 { value = "rush" };
      Commit_transient "_trns";
    ];
  test {|"rush"::bytes|}
    [
      Create_transient "_trns";
      Add_string_bytes { value = "rush" };
      Commit_transient "_trns";
    ];
  test {|"rush"::bytes(3)|}
    [
      Create_transient "_trns";
      Add_string_dynamic_bytes { value = "rush"; expr = int 3 };
      Commit_transient "_trns";
    ];
  test "len::8, body::bytes(len)"
    [
      Create_transient "_trns";
      Add_next_fixed_bits { src = "len"; size = 8 };
      Add_next_dynamic_bytes { src = "body"; expr = id "len" };
      Commit_transient "_trns";
    ];
  test "one::8, all::bytes"
    [
      Create_transient "_trns";
      Add_next_fixed_bits { src = "one"; size = 8 };
      Add_rest { src = "all" };
      Commit_transient "_trns";
    ];

  test
    {| fin::1, comp::1, _rsv::2,
       1::4, 0::1, 127::7,
       len::bytes(8), mask::32,
       payload::bytes(len), rest |}
    [
      Create_transient "_trns";
      Add_next_fixed_bits { src = "fin"; size = 1 };
      Add_next_fixed_bits { src = "comp"; size = 1 };
      Add_next_fixed_bits { src = "_rsv"; size = 2 };
      Add_int_fixed_bits { value = 1; size = 4 };
      Add_int_fixed_bits { value = 0; size = 1 };
      Add_int_fixed_bits { value = 127; size = 7 };
      Add_next_dynamic_bytes { src = "len"; expr = int 8 };
      Add_next_fixed_bits { src = "mask"; size = 32 };
      Add_next_dynamic_bytes { src = "payload"; expr = id "len" };
      Add_rest { src = "rest" };
      Commit_transient "_trns";
    ];
  ()

(**
   Bytestring construction tests
 *)
let () =
  let loc = Location.none in
  let test str expected =
    let lower = Bytepattern.parse str in
    let actual = lower |> Bytepattern.to_transient_builder ~loc in
    let actual = Ppxlib.Pprintast.string_of_expression actual in
    let expected = Ppxlib.Pprintast.string_of_expression expected in
    if not (String.equal actual expected) then (
      Format.printf
        "Error AST doesn't match, expected:\n\n%s\n\nbut found:\n\n%s\n\n"
        expected actual;
      assert false)
    else Format.printf "transl test %S OK\r\n%!" str
  in

  (* test: empty pattern is just an empty bytestring *)
  test {| |} [%expr Bytestring.empty];
  test {| all |} [%expr all];
  test {| all::bytes |} [%expr all];
  test {| all::8 |}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:8 all;
      Bytestring.Transient.commit _trns];
  test "all::1024"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:1024 all;
      Bytestring.Transient.commit _trns];
  test "all::utf8"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_utf8 _trns all;
      Bytestring.Transient.commit _trns];
  test "all::bytes(10)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:10 all;
      Bytestring.Transient.commit _trns];
  test "len::8 , body::bytes(len)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:8 len;
      Bytestring.Transient.add_string _trns ~size:len body;
      Bytestring.Transient.commit _trns];
  test "one::8, all ::bytes"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:8 one;
      Bytestring.Transient.add_string _trns all;
      Bytestring.Transient.commit _trns];
  test "2112::1"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_int _trns ~size:1 2112;
      Bytestring.Transient.commit _trns];
  test "2112::bits(1234)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_int _trns ~size:1234 2112;
      Bytestring.Transient.commit _trns];
  test "2112::bytes(1234)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_int _trns ~size:(1234 * 8) 2112;
      Bytestring.Transient.commit _trns];

  test {|"rush"::utf8|}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_utf8 _trns "rush";
      Bytestring.Transient.commit _trns];
  test {|"rush"::bytes|}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_string _trns "rush";
      Bytestring.Transient.commit _trns];
  test {|"rush"::bytes(3)|}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_literal_string _trns ~size:3 "rush";
      Bytestring.Transient.commit _trns];

  test
    {| fin::1, comp::1, _rsv::2,
        1::4, 0::1, 127::7,
        len::bits(8*8), mask::32,
        payload::bytes(len), rest |}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:1 fin;
      Bytestring.Transient.add_bits _trns ~size:1 comp;
      Bytestring.Transient.add_bits _trns ~size:2 _rsv;
      Bytestring.Transient.add_literal_int _trns ~size:4 1;
      Bytestring.Transient.add_literal_int _trns ~size:1 0;
      Bytestring.Transient.add_literal_int _trns ~size:7 127;
      Bytestring.Transient.add_bits _trns ~size:(8 * 8) len;
      Bytestring.Transient.add_bits _trns ~size:32 mask;
      Bytestring.Transient.add_string _trns ~size:len payload;
      Bytestring.Transient.add_string _trns rest;
      Bytestring.Transient.commit _trns];
  ()

let () =
  let open Matching_lower in
  let test str expected =
    let actual = parse str |> Matching_lower.lower in
    let actual_str = Format.asprintf "%a" Matching_lower.pp actual in
    let expect_str = Format.asprintf "%a" Matching_lower.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "match-low test %S OK\r\n%!" str
    else (
      Format.printf
        "Error %S – lowered repr doesn't match, expected:\n\n\
         %a\n\n\
         but found:\n\n\
         %a\n\n"
        str Matching_lower.pp expected Matching_lower.pp actual;
      assert false)
  in

  test "" [ Empty "_data_src" ];
  test "all" [ Bypass { src = "_data_src"; name = "all" } ];
  test "all::8"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "all"; size = 8; iter = "_data_src" };
    ];
  test "all::1024"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "all"; size = 1024; iter = "_data_src" };
    ];
  test "all::utf8"
    [
      Create_iterator "_data_src";
      Bind_next_utf8 { src = "all"; iter = "_data_src" };
    ];
  test "all::bytes" [ Bypass { src = "_data_src"; name = "all" } ];
  test "all::bytes(10)"
    [
      Create_iterator "_data_src";
      Bind_next_dynamic_bytes { src = "all"; expr = int 10; iter = "_data_src" };
    ];

  test "2112::1"
    [
      Create_iterator "_data_src";
      Expect_int_fixed_bits { value = 2112; size = 1; iter = "_data_src" };
    ];
  test "2112::bits(1234)"
    [
      Create_iterator "_data_src";
      Expect_int_dynamic_bits
        { value = 2112; expr = int 1234; iter = "_data_src" };
    ];
  test "2112::bytes(1234)"
    [
      Create_iterator "_data_src";
      Expect_int_dynamic_bytes
        { value = 2112; expr = int 1234; iter = "_data_src" };
    ];
  test {|"rush"::utf8|}
    [
      Create_iterator "_data_src";
      Expect_string_utf8 { value = "rush"; iter = "_data_src" };
    ];
  test {|"rush"::bytes|}
    [
      Create_iterator "_data_src";
      Expect_string_bytes { value = "rush"; iter = "_data_src" };
    ];
  test {|"rush"::bytes(3)|}
    [
      Create_iterator "_data_src";
      Expect_string_dynamic_bytes
        { value = "rush"; expr = int 3; iter = "_data_src" };
    ];
  test "len::8, body::bytes(len)"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "len"; size = 8; iter = "_data_src" };
      Bind_next_dynamic_bytes
        { src = "body"; expr = id "len"; iter = "_data_src" };
    ];
  test "one::8, all::bytes"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "one"; size = 8; iter = "_data_src" };
      Bind_rest { src = "all"; iter = "_data_src" };
    ];

  test
    {| fin::1, comp::1, _rsv::2,
       1::4, 0::1, 127::7,
       len::bytes(8), mask::32,
       payload::bytes(len), rest |}
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "fin"; size = 1; iter = "_data_src" };
      Bind_next_fixed_bits { src = "comp"; size = 1; iter = "_data_src" };
      Bind_next_fixed_bits { src = "_rsv"; size = 2; iter = "_data_src" };
      Expect_int_fixed_bits { value = 1; size = 4; iter = "_data_src" };
      Expect_int_fixed_bits { value = 0; size = 1; iter = "_data_src" };
      Expect_int_fixed_bits { value = 127; size = 7; iter = "_data_src" };
      Bind_next_dynamic_bytes { src = "len"; expr = int 8; iter = "_data_src" };
      Bind_next_fixed_bits { src = "mask"; size = 32; iter = "_data_src" };
      Bind_next_dynamic_bytes
        { src = "payload"; expr = id "len"; iter = "_data_src" };
      Bind_rest { src = "rest"; iter = "_data_src" };
    ];
  ()

(**
   Bytestring matching tests
 *)
let () =
  let loc = Location.none in
  let test str expected =
    let lower = Bytepattern.parse str in
    let actual = lower |> Bytepattern.to_pattern_match ~loc ~body:[%expr ()] in
    let actual = Ppxlib.Pprintast.string_of_expression actual in
    let expected = Ppxlib.Pprintast.string_of_expression expected in
    if not (String.equal actual expected) then (
      Format.printf
        "Error on %S AST doesn't match, expected:\n\n%s\n\nbut found:\n\n%s\n\n"
        str expected actual;
      assert false)
    else Format.printf "match test %S OK\r\n%!" str
  in

  (* test: empty pattern is just an empty bytestring *)
  test {| |} [%expr Bytestring.assert_empty _data_src];
  test {| all |}
    [%expr
      let all = _data_src in
      ()];
  test {| all::bytes |}
    [%expr
      let all = _data_src in
      ()];
  test {| all::8 |}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bits ~size:8 _data_src in
      ()];
  test "all::1024"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bits ~size:1024 _data_src in
      ()];
  test "all::utf8"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_utf8 _data_src in
      ()];
  test "all::bytes(10)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bytes ~size:10 _data_src in
      ()];
  test "len::8 , body::bytes(len)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let len = Bytestring.Iter.next_bits ~size:8 _data_src in
      let body = Bytestring.Iter.next_bytes ~size:len _data_src in
      ()];
  test "one::8, all ::bytes"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let one = Bytestring.Iter.next_bits ~size:8 _data_src in
      let all = Bytestring.Iter.rest _data_src in
      ()];
  test "2112::1"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:1 2112;

      ()];
  test "2112::bits(1234)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:1234 2112;
      () ()];
  test "2112::bytes(1234)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:(1234 * 8) 2112;
      () ()];

  test {|"rush"::utf8|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_utf8 _data_src "rush";
      () ()];
  test {|"rush"::bytes|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_string _data_src "rush";
      () ()];
  test {|"rush"::bytes(3)|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_string _data_src ~size:3 "rush";
      () ()];
  test
    {| fin::1, comp::1, _rsv::2,
        1::4, 0::1, 127::7,
        len::bits(8*8), mask::32,
        payload::bytes(len), rest |}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
      let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
      let _rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
      Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
      Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
      let len = Bytestring.Iter.next_bits ~size:(8 * 8) _data_src in
      let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
      let payload = Bytestring.Iter.next_bytes ~size:len _data_src in
      let rest = Bytestring.Iter.rest _data_src in

      ()]
