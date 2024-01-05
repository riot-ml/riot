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
      Create_iter "all";
      Add_next_fixed_bits { iter = "all"; size = 8 };
      Commit_transient "_trns";
    ];
  test "all::1024"
    [
      Create_transient "_trns";
      Create_iter "all";
      Add_next_fixed_bits { iter = "all"; size = 1024 };
      Commit_transient "_trns";
    ];
  test "all::utf8"
    [
      Create_transient "_trns";
      Create_iter "all";
      Add_next_utf8 { iter = "all" };
      Commit_transient "_trns";
    ];
  test "all::bytes" [ Bypass "all" ];
  test "all::bytes(10)"
    [
      Create_transient "_trns";
      Create_iter "all";
      Add_next_dynamic_bytes { iter = "all"; expr = int 10 };
      Commit_transient "_trns";
    ];
  test "len::8, body::bytes(len)"
    [
      Create_transient "_trns";
      Create_iter "body";
      Create_iter "len";
      Add_next_fixed_bits { iter = "len"; size = 8 };
      Add_next_dynamic_bytes { iter = "body"; expr = id "len" };
      Commit_transient "_trns";
    ];
  test "one::8, all::bytes"
    [
      Create_transient "_trns";
      Create_iter "all";
      Create_iter "one";
      Add_next_fixed_bits { iter = "one"; size = 8 };
      Add_rest { iter = "all" };
      Commit_transient "_trns";
    ];

  test
    {| fin::1, comp::1, _rsv::2,
       1::4, 0::1, 127::7,
       len::bytes(8), mask::32,
       payload::bytes(len), rest |}
    [
      Create_transient "_trns";
      Create_iter "_rsv";
      Create_iter "comp";
      Create_iter "fin";
      Create_iter "len";
      Create_iter "mask";
      Create_iter "payload";
      Create_iter "rest";
      Add_next_fixed_bits { iter = "fin"; size = 1 };
      Add_next_fixed_bits { iter = "comp"; size = 1 };
      Add_next_fixed_bits { iter = "_rsv"; size = 2 };
      Add_literal_int { value = 1 };
      Add_literal_int { value = 0 };
      Add_literal_int { value = 127 };
      Add_next_dynamic_bytes { iter = "len"; expr = int 8 };
      Add_next_fixed_bits { iter = "mask"; size = 32 };
      Add_next_dynamic_bytes { iter = "payload"; expr = id "len" };
      Add_rest { iter = "rest" };
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
      let _iter_all = Bytestring.to_iter all in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:8 _iter_all);
      Bytestring.Transient.commit _trns];
  test "all::1024"
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter_all = Bytestring.to_iter all in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:1024 _iter_all);
      Bytestring.Transient.commit _trns];
  test "all::utf8"
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter_all = Bytestring.to_iter all in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_utf8 _iter_all);
      Bytestring.Transient.commit _trns];
  test "all::bytes(10)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter_all = Bytestring.to_iter all in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bytes ~size:10 _iter_all);
      Bytestring.Transient.commit _trns];
  test "len::8 , body::bytes(len)"
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter_body = Bytestring.to_iter body in
      let _iter_len = Bytestring.to_iter len in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:8 _iter_len);
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bytes ~size:len _iter_body);
      Bytestring.Transient.commit _trns];
  test "one::8, all ::bytes"
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter_all = Bytestring.to_iter all in
      let _iter_one = Bytestring.to_iter one in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:8 _iter_one);
      Bytestring.Transient.add_string _trns (Bytestring.Iter.rest _iter_all);
      Bytestring.Transient.commit _trns];
  test
    {| fin::1, comp::1, _rsv::2,
        1::4, 0::1, 127::7,
        len::bytes(8), mask::32,
        payload::bytes(len), rest |}
    [%expr
      let _trns = Bytestring.Transient.create () in
      let _iter__rsv = Bytestring.to_iter _rsv in
      let _iter_comp = Bytestring.to_iter comp in
      let _iter_fin = Bytestring.to_iter fin in
      let _iter_len = Bytestring.to_iter len in
      let _iter_mask = Bytestring.to_iter mask in
      let _iter_payload = Bytestring.to_iter payload in
      let _iter_rest = Bytestring.to_iter rest in
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:1 _iter_fin);
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:1 _iter_comp);
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:2 _iter__rsv);
      Bytestring.Transient.add_literal_int _trns int;
      Bytestring.Transient.add_literal_int _trns int;
      Bytestring.Transient.add_literal_int _trns int;
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bytes ~size:8 _iter_len);
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bits ~size:32 _iter_mask);
      Bytestring.Transient.add_string _trns
        (Bytestring.Iter.next_bytes ~size:len _iter_payload);
      Bytestring.Transient.add_string _trns (Bytestring.Iter.rest _iter_rest);
      Bytestring.Transient.commit _trns];
  ()
