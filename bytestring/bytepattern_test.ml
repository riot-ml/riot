open Ppxlib
open Ast_helper
open Bytepattern

let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt
let loc = Location.none

let id name =
  let longident = Location.{ loc; txt = Longident.parse name } in
  Exp.ident longident

let int n = Exp.constant (Const.int n)

let ocaml str =
  let lexbuf = Lexing.from_string ~with_positions:false str in
  Parse.expression lexbuf

let () =
  let test str expected =
    let actual = Lexer.read ~loc str in

    let actual_str = Format.asprintf "%a" Lexer.pp actual in
    let expect_str = Format.asprintf "%a" Lexer.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "lexer test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Tokens do not match")
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

  test "all::bytes(10)"
    [ IDENT "all"; COLON_COLON; IDENT "bytes"; EXPRESSION (int 10) ];

  test "all::bytes(10),"
    [ IDENT "all"; COLON_COLON; IDENT "bytes"; EXPRESSION (int 10); COMMA ];

  test "all::bytes(foo)"
    [ IDENT "all"; COLON_COLON; IDENT "bytes"; EXPRESSION (id "foo") ];

  test "all::bytes(foo 1234)"
    [ IDENT "all"; COLON_COLON; IDENT "bytes"; EXPRESSION (ocaml "foo 1234") ];

  test "all::bytes(foo ())"
    [ IDENT "all"; COLON_COLON; IDENT "bytes"; EXPRESSION (ocaml "foo ()") ];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [
      IDENT "all";
      COLON_COLON;
      IDENT "bytes";
      EXPRESSION (ocaml "((foo ()) * 8) / (10 - 1)");
    ];

  test "all::bytes(10), rest::binary"
    [
      IDENT "all";
      COLON_COLON;
      IDENT "bytes";
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
    let actual = Parser.parse ~loc str in

    let actual_str = Format.asprintf "%a" Parser.pp actual in
    let expect_str = Format.asprintf "%a" Parser.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "parser test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Parse trees do not match")
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
  test "all::bytes(foo ())"
    [ Bind { name = "all"; size = Dynamic_bytes (ocaml "foo ()") } ];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [
      Bind
        {
          name = "all";
          size = Dynamic_bytes (ocaml "((foo ()) * 8) / (10 - 1)");
        };
    ];
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
    let actual = parse ~loc str |> Construction_lower.lower ~loc in
    let actual_str = Format.asprintf "%a" Construction_lower.pp actual in
    let expect_str = Format.asprintf "%a" Construction_lower.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "cstr-low test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Construction_lowered trees do not match")
        Construction_lower.pp expected Construction_lower.pp actual;
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
  test "all::bytes(foo ())"
    [
      Create_transient "_trns";
      Add_next_dynamic_bytes { src = "all"; expr = ocaml "foo ()" };
      Commit_transient "_trns";
    ];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [
      Create_transient "_trns";
      Add_next_dynamic_bytes
        { src = "all"; expr = ocaml "((foo ()) * 8) / (10 - 1)" };
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
  let test str expected =
    let lower = Bytepattern.parse ~loc str in
    let actual = lower |> Bytepattern.to_transient_builder ~loc in
    let actual = Ppxlib.Pprintast.string_of_expression actual in
    let expected = Ppxlib.Pprintast.string_of_expression expected in
    if not (String.equal actual expected) then (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "Construction ASTs do not match")
        expected actual;
      assert false)
    else Format.printf "transl-low test %S %s\r\n%!" str (keyword "OK")
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
  test "all::bytes(foo ())"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:(foo ()) all;
      Bytestring.Transient.commit _trns];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_string _trns ~size:(foo () * 8 / (10 - 1)) all;
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
    let actual = parse ~loc str |> Matching_lower.lower ~loc in
    let actual_str = Format.asprintf "%a" Matching_lower.pp actual in
    let expect_str = Format.asprintf "%a" Matching_lower.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "match-low test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Matching_lower trees do not match")
        Matching_lower.pp expected Matching_lower.pp actual;
      assert false)
  in

  test "" [ Empty "_data_src" ];
  test "all" [ Bypass { src = "_data_src"; name = "all" } ];
  test "all::8"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "all"; size = 8; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "all::1024"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "all"; size = 1024; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "all::utf8"
    [
      Create_iterator "_data_src";
      Bind_next_utf8 { src = "all"; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "all::bytes" [ Bypass { src = "_data_src"; name = "all" } ];
  test "all::bytes(10)"
    [
      Create_iterator "_data_src";
      Bind_next_dynamic_bytes { src = "all"; expr = int 10; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "all::bytes(foo ())"
    [
      Create_iterator "_data_src";
      Bind_next_dynamic_bytes
        { src = "all"; expr = ocaml "foo ()"; iter = "_data_src" };
      Empty "_data_src";
    ];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [
      Create_iterator "_data_src";
      Bind_next_dynamic_bytes
        {
          src = "all";
          expr = ocaml "((foo ()) * 8) / (10 - 1)";
          iter = "_data_src";
        };
      Empty "_data_src";
    ];

  test "2112::1"
    [
      Create_iterator "_data_src";
      Expect_int_fixed_bits { value = 2112; size = 1; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "2112::bits(1234)"
    [
      Create_iterator "_data_src";
      Expect_int_dynamic_bits
        { value = 2112; expr = int 1234; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "2112::bytes(1234)"
    [
      Create_iterator "_data_src";
      Expect_int_dynamic_bytes
        { value = 2112; expr = int 1234; iter = "_data_src" };
      Empty "_data_src";
    ];
  test {|"rush"::utf8|}
    [
      Create_iterator "_data_src";
      Expect_string_utf8 { value = "rush"; iter = "_data_src" };
      Empty "_data_src";
    ];
  test {|"rush"::bytes|}
    [
      Create_iterator "_data_src";
      Expect_string_bytes { value = "rush"; iter = "_data_src" };
      Empty "_data_src";
    ];
  test {|"rush"::bytes(3)|}
    [
      Create_iterator "_data_src";
      Expect_string_dynamic_bytes
        { value = "rush"; expr = int 3; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "len::8, body::bytes(len)"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "len"; size = 8; iter = "_data_src" };
      Bind_next_dynamic_bytes
        { src = "body"; expr = id "len"; iter = "_data_src" };
      Empty "_data_src";
    ];
  test "one::8, all::bytes"
    [
      Create_iterator "_data_src";
      Bind_next_fixed_bits { src = "one"; size = 8; iter = "_data_src" };
      Bind_rest { src = "all"; iter = "_data_src" };
      Empty "_data_src";
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
      Empty "_data_src";
    ];
  ()

(**
   Bytestring matching tests
 *)
let () =
  let loc = Location.none in
  let test str expected =
    let lower = Bytepattern.parse ~loc str in
    let actual = lower |> Bytepattern.to_pattern_match ~loc ~body:[%expr ()] in
    let actual = Ppxlib.Pprintast.string_of_expression actual in
    let expected = Ppxlib.Pprintast.string_of_expression expected in
    if not (String.equal actual expected) then (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "Native matching trees do not match")
        expected actual;
      assert false)
    else Format.printf "match-native test %S %s\r\n%!" str (keyword "OK")
  in

  (* test: empty pattern is just an empty bytestring *)
  test {| |}
    [%expr
      Bytestring.Iter.expect_empty _data_src;
      ()];
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
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "all::1024"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bits ~size:1024 _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "all::utf8"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_utf8 _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "all::bytes(10)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bytes ~size:10 _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "all::bytes(foo ())"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all = Bytestring.Iter.next_bytes ~size:(foo ()) _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];

  test "all::bytes( (foo ()) * 8 / ( 10 - 1 ) )"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let all =
        Bytestring.Iter.next_bytes ~size:(foo () * 8 / (10 - 1)) _data_src
      in
      Bytestring.Iter.expect_empty _data_src;
      ()];

  test "len::8 , body::bytes(len)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let len = Bytestring.Iter.next_bits ~size:8 _data_src in
      let body = Bytestring.Iter.next_bytes ~size:len _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "one::8, all ::bytes"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      let one = Bytestring.Iter.next_bits ~size:8 _data_src in
      let all = Bytestring.Iter.rest _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "2112::1"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:1 2112;
      Bytestring.Iter.expect_empty _data_src;

      ()];
  test "2112::bits(1234)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:1234 2112;
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test "2112::bytes(1234)"
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_int _data_src ~size:(1234 * 8) 2112;
      Bytestring.Iter.expect_empty _data_src;
      ()];

  test {|"rush"::utf8|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_utf8 _data_src "rush";
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test {|"rush"::bytes|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_string _data_src "rush";
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test {|"rush"::bytes(3)|}
    [%expr
      let _data_src = Bytestring.to_iter _data_src in
      Bytestring.Iter.expect_literal_string _data_src ~size:3 "rush";
      Bytestring.Iter.expect_empty _data_src;
      ()];
  test
    {| fin::1, comp::1, _rsv::2,
        1::4, 0::1, 127::7,
        len::bits(8*8), mask::32,
        payload::bytes(foo len), rest |}
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
      let payload = Bytestring.Iter.next_bytes ~size:(foo len) _data_src in
      let rest = Bytestring.Iter.rest _data_src in
      Bytestring.Iter.expect_empty _data_src;
      ()]

(**
   Bytestring prefix optimizatoin tests
 *)
let () =
  let loc = Location.none in
  let test ?guard n strs expected =
    let lowers =
      List.mapi
        (fun idx str ->
          let guard = if idx = 0 then guard else None in
          let test_name =
            id ("test_" ^ string_of_int n ^ "_body_" ^ string_of_int idx)
          in
          (Bytepattern.parse ~loc str, guard, [%expr [%e test_name]]))
        strs
    in
    let actual = Bytepattern.to_prefix_match ~loc lowers in
    let actual =
      Format.asprintf "%a" Bytepattern.Prefix_matching.pp [ actual ]
    in
    let expect = Format.asprintf "%a" Bytepattern.Prefix_matching.pp expected in
    if not (String.equal actual expect) then (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "#%d Prefix matching trees do not match" n)
        expect actual;
      assert false)
    else Format.printf "match-prefix test #%d %s\r\n%!" n (keyword "OK")
  in

  (* test: empty pattern is just an empty bytestring *)
  test 0 [ {| |}; {| |} ]
    [
      Prefix
        ( [ Empty "_data_src" ],
          [
            Try_run ([], (None, id "test_0_body_0"));
            Try_run ([], (None, id "test_0_body_1"));
          ] );
    ];

  test 1
    [
      {| hello::8, byte::2, other::3 |}; {| hello::8,byte::2 |}; {| world::10 |};
    ]
    [
      Prefix
        ( [ Create_iterator "_data_src" ],
          [
            Prefix
              ( [
                  Bind_next_fixed_bits
                    { src = "hello"; size = 8; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "byte"; size = 2; iter = "_data_src" };
                ],
                [
                  Try_run
                    ( [
                        Bind_next_fixed_bits
                          { src = "other"; size = 3; iter = "_data_src" };
                        Empty "_data_src";
                      ],
                      (None, id "test_1_body_0") );
                  Try_run ([ Empty "_data_src" ], (None, id "test_1_body_1"));
                ] );
            Try_run
              ( [
                  Bind_next_fixed_bits
                    { src = "world"; size = 10; iter = "_data_src" };
                  Empty "_data_src";
                ],
                (None, id "test_1_body_2") );
          ] );
    ];

  test 2
    [
      {| fin::1, comp::1, 0::2, 1::4, 0::1, 127::7,
         len::bits(compute_bits ()), _mask::32, _payload::bytes(len), rest, |};
      {| fin::1, comp::1, 0::2, 1::4, 0::1, 127::7, |};
      {| fin::1, comp::1 |};
      {| rest |};
    ]
    [
      Prefix
        ( [],
          [
            Prefix
              ( [
                  Create_iterator "_data_src";
                  Bind_next_fixed_bits
                    { src = "fin"; size = 1; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "comp"; size = 1; iter = "_data_src" };
                ],
                [
                  Prefix
                    ( [
                        Expect_int_fixed_bits
                          { value = 0; size = 2; iter = "_data_src" };
                        Expect_int_fixed_bits
                          { value = 1; size = 4; iter = "_data_src" };
                        Expect_int_fixed_bits
                          { value = 0; size = 1; iter = "_data_src" };
                        Expect_int_fixed_bits
                          { value = 127; size = 7; iter = "_data_src" };
                      ],
                      [
                        Try_run
                          ( [
                              Bind_next_dynamic_bits
                                {
                                  src = "len";
                                  expr = ocaml "compute_bits ()";
                                  iter = "_data_src";
                                };
                              Bind_next_fixed_bits
                                { src = "_mask"; size = 32; iter = "_data_src" };
                              Bind_next_dynamic_bytes
                                {
                                  src = "_payload";
                                  expr = ocaml "len";
                                  iter = "_data_src";
                                };
                              Bind_rest { src = "rest"; iter = "_data_src" };
                              Empty "_data_src";
                            ],
                            (None, id "test_2_body_0") );
                        Try_run
                          ([ Empty "_data_src" ], (None, id "test_2_body_1"));
                      ] );
                  Try_run ([ Empty "_data_src" ], (None, id "test_2_body_2"));
                ] );
            Try_run
              ( [ Bypass { src = "_data_src"; name = "rest" } ],
                (None, id "test_2_body_3") );
          ] );
    ];

  test 3 ~guard:[%expr run_guard ()]
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| data :: bytes |};
    ]
    [
      Prefix
        ( [],
          [
            Prefix
              ( [
                  Create_iterator "_data_src";
                  Bind_next_fixed_bits
                    { src = "fin"; size = 1; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "compressed"; size = 1; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "rsv"; size = 2; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "opcode"; size = 4; iter = "_data_src" };
                  Expect_int_fixed_bits
                    { value = 1; size = 1; iter = "_data_src" };
                  Expect_int_fixed_bits
                    { value = 127; size = 7; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "length"; size = 64; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "mask"; size = 32; iter = "_data_src" };
                  Bind_next_dynamic_bytes
                    {
                      src = "payload";
                      expr = ocaml "length";
                      iter = "_data_src";
                    };
                  Bind_rest { src = "rest"; iter = "_data_src" };
                  Empty "_data_src";
                ],
                [
                  Try_run ([], (Some (ocaml "run_guard ()"), id "test_3_body_0"));
                  Try_run ([], (None, id "test_3_body_1"));
                  Try_run ([], (None, id "test_3_body_2"));
                ] );
            Try_run
              ( [ Bypass { src = "_data_src"; name = "data" } ],
                (None, id "test_3_body_3") );
          ] );
    ];

  test 4 ~guard:[%expr run_guard ()]
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
    ]
    [
      Try_run
        ( [
            Create_iterator "_data_src";
            Bind_next_fixed_bits { src = "fin"; size = 1; iter = "_data_src" };
            Bind_next_fixed_bits
              { src = "compressed"; size = 1; iter = "_data_src" };
            Bind_next_fixed_bits { src = "rsv"; size = 2; iter = "_data_src" };
            Bind_next_fixed_bits
              { src = "opcode"; size = 4; iter = "_data_src" };
            Expect_int_fixed_bits { value = 1; size = 1; iter = "_data_src" };
            Expect_int_fixed_bits { value = 127; size = 7; iter = "_data_src" };
            Bind_next_fixed_bits
              { src = "length"; size = 64; iter = "_data_src" };
            Bind_next_fixed_bits { src = "mask"; size = 32; iter = "_data_src" };
            Bind_next_dynamic_bytes
              { src = "payload"; expr = ocaml "length"; iter = "_data_src" };
            Bind_rest { src = "rest"; iter = "_data_src" };
            Empty "_data_src";
          ],
          (Some (ocaml "run_guard ()"), id "test_4_body_0") );
    ];

  test 5
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 0 :: 1, 126 :: 7,
         length :: 16, mask :: 32, payload :: bytes(length * 8), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 0 :: 1,
         length :: 7, mask :: 32, payload :: bytes(length * 8), rest :: bytes |};
      {| data :: bytes |};
    ]
    [
      Prefix
        ( [],
          [
            Prefix
              ( [
                  Create_iterator "_data_src";
                  Bind_next_fixed_bits
                    { src = "fin"; size = 1; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "compressed"; size = 1; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "rsv"; size = 2; iter = "_data_src" };
                  Bind_next_fixed_bits
                    { src = "opcode"; size = 4; iter = "_data_src" };
                ],
                [
                  Try_run
                    ( [
                        Expect_int_fixed_bits
                          { value = 1; size = 1; iter = "_data_src" };
                        Expect_int_fixed_bits
                          { value = 127; size = 7; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "length"; size = 64; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "mask"; size = 32; iter = "_data_src" };
                        Bind_next_dynamic_bytes
                          {
                            src = "payload";
                            expr = ocaml "length";
                            iter = "_data_src";
                          };
                        Bind_rest { src = "rest"; iter = "_data_src" };
                        Empty "_data_src";
                      ],
                      (None, id "test_5_body_0") );
                  Try_run
                    ( [
                        Expect_int_fixed_bits
                          { value = 0; size = 1; iter = "_data_src" };
                        Expect_int_fixed_bits
                          { value = 126; size = 7; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "length"; size = 16; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "mask"; size = 32; iter = "_data_src" };
                        Bind_next_dynamic_bytes
                          {
                            src = "payload";
                            expr = ocaml "length * 8";
                            iter = "_data_src";
                          };
                        Bind_rest { src = "rest"; iter = "_data_src" };
                        Empty "_data_src";
                      ],
                      (None, id "test_5_body_1") );
                  Try_run
                    ( [
                        Expect_int_fixed_bits
                          { value = 0; size = 1; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "length"; size = 7; iter = "_data_src" };
                        Bind_next_fixed_bits
                          { src = "mask"; size = 32; iter = "_data_src" };
                        Bind_next_dynamic_bytes
                          {
                            src = "payload";
                            expr = ocaml "length * 8";
                            iter = "_data_src";
                          };
                        Bind_rest { src = "rest"; iter = "_data_src" };
                        Empty "_data_src";
                      ],
                      (None, id "test_5_body_2") );
                ] );
            Try_run
              ( [ Bypass { src = "_data_src"; name = "data" } ],
                (None, id "test_5_body_3") );
          ] );
    ];

  ()

(**
   Bytestring prefix matching tests
 *)
let () =
  let loc = Location.none in
  let test ?guard n strs expected =
    let lowers =
      List.mapi
        (fun idx str ->
          let guard = if idx = 0 then guard else None in
          let test_name =
            id ("test_" ^ string_of_int n ^ "_body_" ^ string_of_int idx)
          in
          (Bytepattern.parse ~loc str, guard, [%expr [%e test_name]]))
        strs
    in
    let actual =
      Bytepattern.to_match_expression ~loc ~data:[%expr data] lowers
    in
    let actual = Ppxlib.Pprintast.string_of_expression actual in
    let expect = Ppxlib.Pprintast.string_of_expression expected in
    if not (String.equal actual expect) then (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "#%d OCaml ASTs do not match" n)
        expect actual;
      assert false)
    else Format.printf "match test #%d %s\r\n%!" n (keyword "OK")
  in

  (* test: empty pattern is just an empty bytestring *)
  test 0 [ {| |}; {| |} ]
    [%expr
      (fun _data_src ->
        Bytestring.Iter.expect_empty _data_src;
        try test_0_body_0
        with Bytestring.No_match -> (
          try test_0_body_1
          with Bytestring.No_match -> raise Bytestring.No_match))
        data];

  test 1
    [
      {| hello::8, byte::2, other::3 |}; {| hello::8,byte::2 |}; {| world::10 |};
    ]
    [%expr
      (fun _data_src ->
        let _data_src = Bytestring.to_iter _data_src in
        try
          let hello = Bytestring.Iter.next_bits ~size:8 _data_src in
          let byte = Bytestring.Iter.next_bits ~size:2 _data_src in
          try
            let other = Bytestring.Iter.next_bits ~size:3 _data_src in
            Bytestring.Iter.expect_empty _data_src;
            test_1_body_0
          with Bytestring.No_match -> (
            try
              Bytestring.Iter.expect_empty _data_src;
              test_1_body_1
            with Bytestring.No_match -> raise Bytestring.No_match)
        with Bytestring.No_match -> (
          try
            let world = Bytestring.Iter.next_bits ~size:10 _data_src in
            Bytestring.Iter.expect_empty _data_src;
            test_1_body_2
          with Bytestring.No_match -> raise Bytestring.No_match))
        data];

  test 2
    [
      {| fin::1, comp::1, 0::2, 1::4, 0::1, 127::7,
         len::bits(compute_bits ()), _mask::32, _payload::bytes(len), rest, |};
      {| fin::1, comp::1, 0::2, 1::4, 0::1, 127::7, |};
      {| fin::1, comp::1 |};
      {| rest |};
    ]
    [%expr
      (fun _data_src ->
        try
          let _data_src = Bytestring.to_iter _data_src in
          let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
          let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
          try
            Bytestring.Iter.expect_literal_int _data_src ~size:2 0;
            Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
            Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
            Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
            try
              let len =
                Bytestring.Iter.next_bits ~size:(compute_bits ()) _data_src
              in
              let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
              let _payload = Bytestring.Iter.next_bytes ~size:len _data_src in
              let rest = Bytestring.Iter.rest _data_src in
              Bytestring.Iter.expect_empty _data_src;
              test_2_body_0
            with Bytestring.No_match -> (
              try
                Bytestring.Iter.expect_empty _data_src;
                test_2_body_1
              with Bytestring.No_match -> raise Bytestring.No_match)
          with Bytestring.No_match -> (
            try
              Bytestring.Iter.expect_empty _data_src;
              test_2_body_2
            with Bytestring.No_match -> raise Bytestring.No_match)
        with Bytestring.No_match -> (
          try
            let rest = _data_src in
            test_2_body_3
          with Bytestring.No_match -> raise Bytestring.No_match))
        data];

  test 3 ~guard:[%expr run_guard ()]
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| data :: bytes |};
    ]
    [%expr
      (fun _data_src ->
        try
          let _data_src = Bytestring.to_iter _data_src in
          let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
          let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
          let rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
          let opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
          Bytestring.Iter.expect_literal_int _data_src ~size:1 1;
          Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
          let length = Bytestring.Iter.next_bits ~size:64 _data_src in
          let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
          let payload = Bytestring.Iter.next_bytes ~size:length _data_src in
          let rest = Bytestring.Iter.rest _data_src in
          Bytestring.Iter.expect_empty _data_src;
          if run_guard () then test_3_body_0 else test_3_body_1
        with Bytestring.No_match -> (
          try
            let data = _data_src in
            test_3_body_2
          with Bytestring.No_match -> raise Bytestring.No_match))
        data];

  test 4 ~guard:[%expr run_guard ()]
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
    ]
    [%expr
      (fun _data_src ->
        let _data_src = Bytestring.to_iter _data_src in
        let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
        let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
        let rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
        let opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
        Bytestring.Iter.expect_literal_int _data_src ~size:1 1;
        Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
        let length = Bytestring.Iter.next_bits ~size:64 _data_src in
        let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
        let payload = Bytestring.Iter.next_bytes ~size:length _data_src in
        let rest = Bytestring.Iter.rest _data_src in
        Bytestring.Iter.expect_empty _data_src;
        if run_guard () then test_4_body_0 else test_4_body_1)
        data];

  test 5
    [
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 1 :: 1, 127 :: 7,
         length :: 64, mask :: 32, payload :: bytes(length), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 0 :: 1, 126 :: 7,
         length :: 16, mask :: 32, payload :: bytes(length * 8), rest :: bytes |};
      {| fin :: 1, compressed :: 1, rsv :: 2, opcode :: 4, 0 :: 1,
         length :: 7, mask :: 32, payload :: bytes(length * 8), rest :: bytes |};
      {| data :: bytes |};
    ]
    [%expr
      (fun _data_src ->
        try
          let _data_src = Bytestring.to_iter _data_src in
          let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
          let compressed = Bytestring.Iter.next_bits ~size:1 _data_src in
          let rsv = Bytestring.Iter.next_bits ~size:2 _data_src in
          let opcode = Bytestring.Iter.next_bits ~size:4 _data_src in
          try
            Bytestring.Iter.expect_literal_int _data_src ~size:1 1;
            Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
            let length = Bytestring.Iter.next_bits ~size:64 _data_src in
            let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
            let payload = Bytestring.Iter.next_bytes ~size:length _data_src in
            let rest = Bytestring.Iter.rest _data_src in
            Bytestring.Iter.expect_empty _data_src;
            test_5_body_0
          with Bytestring.No_match -> (
            try
              Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
              Bytestring.Iter.expect_literal_int _data_src ~size:7 126;
              let length = Bytestring.Iter.next_bits ~size:16 _data_src in
              let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
              let payload =
                Bytestring.Iter.next_bytes ~size:(length * 8) _data_src
              in
              let rest = Bytestring.Iter.rest _data_src in
              Bytestring.Iter.expect_empty _data_src;
              test_5_body_1
            with Bytestring.No_match -> (
              try
                Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
                let length = Bytestring.Iter.next_bits ~size:7 _data_src in
                let mask = Bytestring.Iter.next_bits ~size:32 _data_src in
                let payload =
                  Bytestring.Iter.next_bytes ~size:(length * 8) _data_src
                in
                let rest = Bytestring.Iter.rest _data_src in
                Bytestring.Iter.expect_empty _data_src;
                test_5_body_2
              with Bytestring.No_match -> raise Bytestring.No_match))
        with Bytestring.No_match -> (
          try
            let data = _data_src in
            test_5_body_3
          with Bytestring.No_match -> raise Bytestring.No_match))
        data];
  ()
