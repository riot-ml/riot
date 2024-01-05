open Bytepattern

let () =
  assert (Lexer.read "" = []);
  assert (Lexer.read "all" = [ IDENT "all" ]);
  assert (Lexer.read "all::4" = [ IDENT "all"; COLON_COLON; NUMBER 4L ]);
  assert (Lexer.read "all::1024" = [ IDENT "all"; COLON_COLON; NUMBER 1024L ]);
  assert (Lexer.read "all::utf8" = [ IDENT "all"; COLON_COLON; IDENT "utf8" ]);

  assert (
    Lexer.read "all::byte(10)"
    = [ IDENT "all"; COLON_COLON; IDENT "byte"; EXPRESSION "10" ]);

  assert (
    Lexer.read "all::byte(10),"
    = [
        IDENT "all";
        COLON_COLON;
        IDENT "byte";
        EXPRESSION "10";
        COMMA;
      ]);

  assert (
    Lexer.read "all::byte(10), rest::binary"
    = [
        IDENT "all";
        COLON_COLON;
        IDENT "byte";
        EXPRESSION "10";
        COMMA;
        IDENT "rest";
        COLON_COLON;
        IDENT "binary";
      ]);

  assert (
    Lexer.read
      {| fin::1, comp::1, _rsv::2, 1::4 , 0::1, 127::7 , len ::64

      ,
      mask::32
,payload::bytes(

  len

  ) 

      |}
    = [
        IDENT "fin";
        COLON_COLON;
        NUMBER 1L;
        COMMA;
        IDENT "comp";
        COLON_COLON;
        NUMBER 1L;
        COMMA;
        IDENT "_rsv";
        COLON_COLON;
        NUMBER 2L;
        COMMA;
        NUMBER 1L;
        COLON_COLON;
        NUMBER 4L;
        COMMA;
        NUMBER 0L;
        COLON_COLON;
        NUMBER 1L;
        COMMA;
        NUMBER 127L;
        COLON_COLON;
        NUMBER 7L;
        COMMA;
        IDENT "len";
        COLON_COLON;
        NUMBER 64L;
        COMMA;
        IDENT "mask";
        COLON_COLON;
        NUMBER 32L;
        COMMA;
        IDENT "payload";
        COLON_COLON;
        IDENT "bytes";
        EXPRESSION "len"

    ]);
    ()
