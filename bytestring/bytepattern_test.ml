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
   Lowered representation of the pattern language.
*)
let () =
  let open Lower in
  let test str expected =
    let actual = parse str |> Lower.lower in
    let actual_str = Format.asprintf "%a" Lower.pp actual in
    let expect_str = Format.asprintf "%a" Lower.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "lower test %S OK\r\n%!" str
    else (
      Format.printf
        "Error %S – lowered repr doesn't match, expected:\n\n\
         %a\n\n\
         but found:\n\n\
         %a\n\n"
        str Lower.pp expected Lower.pp actual;
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
    let lower = Bytepattern.parse str |> Bytepattern.lower in
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
  test
    {| fin::1, comp::1, _rsv::2,
        1::4, 0::1, 127::7,
        len::bytes(8), mask::32,
        payload::bytes(len), rest |}
    [%expr
      let _trns = Bytestring.Transient.create () in
      Bytestring.Transient.add_bits _trns ~size:1 fin;
      Bytestring.Transient.add_bits _trns ~size:1 comp;
      Bytestring.Transient.add_bits _trns ~size:2 _rsv;
      Bytestring.Transient.add_literal_int _trns ~size:4 1;
      Bytestring.Transient.add_literal_int _trns ~size:1 0;
      Bytestring.Transient.add_literal_int _trns ~size:7 127;
      Bytestring.Transient.add_string _trns ~size:8 len;
      Bytestring.Transient.add_bits _trns ~size:32 mask;
      Bytestring.Transient.add_string _trns ~size:len payload;
      Bytestring.Transient.add_string _trns rest;
      Bytestring.Transient.commit _trns];
  ()
