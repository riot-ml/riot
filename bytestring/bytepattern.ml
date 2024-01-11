open Ppxlib

let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt
let mint = Spices.color "#77e5b7"
let keyword fmt = Spices.(default |> fg mint |> build) fmt

type error = { loc : Location.t; error : string }

exception Error of error

let failwith ~loc error = raise (Error { loc; error = error ^ "\n" })

let bug ~loc reason =
  failwith ~loc
    (Format.sprintf
       {|Oops! This is a bug. We should never get here, please file an issue here \

https://github.com/leostera/riot/issues/new

Contenxt: %s |}
       reason)

let log = Printf.printf

type token =
  | IDENT of string
  | COMMA
  | COLON_COLON
  | EOF
  | NUMBER of int
  | STRING of string
  | EXPRESSION of Parsetree.expression

type value = String of string | Number of int

type size =
  | Fixed_bits of int
  | Dynamic_bytes of Parsetree.expression
  | Dynamic_bits of Parsetree.expression
  | Dynamic_utf8 of Parsetree.expression
  | Utf8
  | Rest

type pattern =
  | Bind of { name : string; size : size }
  | Expect of { value : value; size : size }

type parsetree = pattern list

module Lexer = struct
  let to_string ts =
    ts
    |> List.map (fun t ->
           match t with
           | IDENT name -> name
           | COMMA -> ","
           | COLON_COLON -> "::"
           | EOF -> ""
           | NUMBER n -> Format.sprintf "%d" n
           | STRING s -> Format.sprintf "%S" s
           | EXPRESSION e ->
               Format.sprintf "%S" (Pprintast.string_of_expression e))
    |> String.concat ""

  let pp_one fmt (t : token) =
    match t with
    | IDENT name -> Format.fprintf fmt "IDENT(%S)" name
    | COMMA -> Format.fprintf fmt "COMMA"
    | COLON_COLON -> Format.fprintf fmt "COLON_COLON"
    | EOF -> Format.fprintf fmt "EOF"
    | NUMBER n -> Format.fprintf fmt "NUMBER(%d)" n
    | STRING s -> Format.fprintf fmt "STRING(%S)" s
    | EXPRESSION e ->
        Format.fprintf fmt "EXPRESSION(%S)" (Pprintast.string_of_expression e)

  let pp fmt t =
    Format.fprintf fmt "[\r\n  ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\r\n  ")
      pp_one fmt t;
    Format.fprintf fmt "\r\n]\r\n"

  let digit = [%sedlex.regexp? Plus '0' .. '9']
  let letter = [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z']
  let ident = [%sedlex.regexp? letter, Star (letter | digit)]

  let rec token ~loc buf acc =
    match%sedlex buf with
    | white_space -> token ~loc buf acc
    | "," ->
        (* log ",%!"; *)
        token ~loc buf (COMMA :: acc)
    | "::" ->
        (* log "::%!"; *)
        token ~loc buf (COLON_COLON :: acc)
    | "(", Star white_space ->
        (* log "(%!"; *)
        expr ~loc ~parens:0 buf acc
    | "\"" ->
        (* log "\"%!"; *)
        string ~loc buf acc
    | ident ->
        let ident = Sedlexing.Utf8.lexeme buf in
        (* log "%s%!" ident; *)
        token ~loc buf (IDENT ident :: acc)
    | digit ->
        let num = Sedlexing.Utf8.lexeme buf in
        (* log "%s%!" num; *)
        token ~loc buf (NUMBER (Int64.of_string num |> Int64.to_int) :: acc)
    | eof ->
        (* log "\n%!"; *)
        acc
    | _ -> failwith ~loc "Unexpected character asdf"

  and string ~loc buf ?(str = []) acc =
    match%sedlex buf with
    | "\"" ->
        let str = List.rev str |> String.concat "" in
        (* log "%s\"%!" str; *)
        token ~loc buf (STRING str :: acc)
    | any ->
        let ident = Sedlexing.Utf8.lexeme buf in
        string ~loc buf ~str:(ident :: str) acc
    | _ -> failwith ~loc "Unexpected character"

  and expr ~loc buf ~parens ?(exp = []) acc =
    match%sedlex buf with
    | ")" ->
        if parens > 0 then
          let ident = Sedlexing.Utf8.lexeme buf in
          expr ~loc buf ~parens:(parens - 1) ~exp:(ident :: exp) acc
        else
          let expr = List.rev exp |> String.concat "" in
          (* log "%s)%!" expr; *)
          let lexbuf = Lexing.from_string ~with_positions:false expr in
          let expr = Parse.expression lexbuf in
          let expr = EXPRESSION expr in
          token ~loc buf (expr :: acc)
    | "(" ->
        let ident = Sedlexing.Utf8.lexeme buf in
        expr ~loc buf ~parens:(parens + 1) ~exp:(ident :: exp) acc
    | any ->
        let ident = Sedlexing.Utf8.lexeme buf in
        expr ~loc buf ~parens ~exp:(ident :: exp) acc
    | _ -> failwith ~loc "Unexpected character"

  let read ~loc str =
    let lexbuf = Sedlexing.Utf8.from_string str in
    token ~loc lexbuf [] |> List.rev
end

module Parser = struct
  let pp_value fmt (v : value) =
    match v with
    | String s -> Format.fprintf fmt "String(%S)" s
    | Number n -> Format.fprintf fmt "Number(%d)" n

  let pp_one fmt (t : pattern) =
    match t with
    | Bind { name; size = Fixed_bits n } ->
        Format.fprintf fmt "Bind { name=%S; size=(Fixed_bits %d) }" name n
    | Bind { name; size = Dynamic_bytes e } ->
        Format.fprintf fmt "Bind { name=%S; size=(Dynamic_bytes %S) }" name
          (Pprintast.string_of_expression e)
    | Bind { name; size = Dynamic_bits e } ->
        Format.fprintf fmt "Bind { name=%S; size=(Dynamic_bits %S) }" name
          (Pprintast.string_of_expression e)
    | Bind { name; size = Dynamic_utf8 e } ->
        Format.fprintf fmt "Bind { name=%S; size=(Dynamic_utf8 %S) }" name
          (Pprintast.string_of_expression e)
    | Bind { name; size = Utf8 } ->
        Format.fprintf fmt "Bind { name=%S; size=Utf8 }" name
    | Bind { name; size = Rest } ->
        Format.fprintf fmt "Bind { name=%S; size=Rest }" name
    | Expect { value; size = Fixed_bits n } ->
        Format.fprintf fmt "Expect { value=%a; size=(Fixed_bits %d) }" pp_value
          value n
    | Expect { value; size = Dynamic_bytes e } ->
        Format.fprintf fmt "Expect { value=%a; size=(Dynamic_bytes %S) }"
          pp_value value
          (Pprintast.string_of_expression e)
    | Expect { value; size = Dynamic_bits e } ->
        Format.fprintf fmt "Expect { value=%a; size=(Dynamic_bits %S) }"
          pp_value value
          (Pprintast.string_of_expression e)
    | Expect { value; size = Dynamic_utf8 e } ->
        Format.fprintf fmt "Expect { value=%a; size=(Dynamic_utf8 %S) }"
          pp_value value
          (Pprintast.string_of_expression e)
    | Expect { value; size = Utf8 } ->
        Format.fprintf fmt "Expect { value=%a; size=Utf8 }" pp_value value
    | Expect { value; size = Rest } ->
        Format.fprintf fmt "Expect { value=%a; size=Rest }" pp_value value

  let pp fmt t =
    Format.fprintf fmt "[\r\n  ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\r\n  ")
      pp_one fmt t;
    Format.fprintf fmt "\r\n]\r\n"

  let bind name size = Bind { name; size }
  let expect value size = Expect { value; size }

  let rec parse ~loc str =
    let tokens = Lexer.read ~loc str in
    if tokens = [] then [] else do_parse ~loc tokens []

  and do_parse ~loc tokens acc =
    match tokens with
    | [] -> List.rev acc
    (* note: support trailing commas, but not just a comma *)
    | [ COMMA ] when List.length acc > 0 -> List.rev acc
    | [ COMMA ] when List.length acc = 0 ->
        failwith ~loc
          (Format.sprintf
             {|%s

A bytestring pattern must have zero, one, or more fields separated by commas.

Trailing commas are supported, but a single comma is not a valid bytestring pattern.
    |}
             (error "Invalid bytestring pattern"))
    | _ ->
        let pattern, rest = parse_pattern ~loc tokens in
        (* log "\n"; *)
        do_parse ~loc rest (pattern :: acc)

  and parse_pattern ~loc (tokens : token list) =
    match tokens with
    | IDENT name :: COLON_COLON :: rest -> (
        (* log "bind %s:: " name; *)
        let size, rest = parse_size ~name ~loc rest in
        match expect_comma ~loc rest with
        | `cont rest -> (bind name size, rest)
        | `halt -> (bind name size, []))
    | IDENT name :: rest ->
        (* log "bind %s with implied rest" name; *)
        (bind name Rest, rest)
    | STRING s :: COLON_COLON :: rest -> (
        (* log "expect %n:: " n; *)
        let value = String s in
        let size, rest = parse_size ~name:s ~loc rest in
        match expect_comma ~loc rest with
        | `cont rest -> (expect value size, rest)
        | `halt -> (expect value size, []))
    | STRING s :: rest ->
        (* log "expect %d with implied size of 1 byte" n; *)
        (expect (String s) (Fixed_bits 8), rest)
    | NUMBER n :: COLON_COLON :: rest -> (
        (* log "expect %n:: " n; *)
        let value = Number n in
        let size, rest = parse_size ~name:(string_of_int n) ~loc rest in
        match expect_comma ~loc rest with
        | `cont rest -> (expect value size, rest)
        | `halt -> (expect value size, []))
    | NUMBER n :: rest ->
        (* log "expect %d with implied size of 1 byte" n; *)
        (expect (Number n) (Fixed_bits 1), rest)
    | _ -> failwith ~loc "patterns must begin with identifiers or constants"

  and parse_size ~name ~loc (tokens : token list) =
    match tokens with
    | IDENT "bytes" :: EXPRESSION expr :: rest ->
        (* log "dynamic bytes(%s)" expr; *)
        (Dynamic_bytes expr, rest)
    | IDENT "bits" :: EXPRESSION expr :: rest ->
        (* log "dynamic bit size(%s)" expr; *)
        (Dynamic_bits expr, rest)
    | IDENT "utf8" :: EXPRESSION expr :: rest ->
        (* log "dynamic utf8(%s)" expr; *)
        (Dynamic_utf8 expr, rest)
    | IDENT "utf8" :: rest ->
        (* log "single utf8 char"; *)
        (Utf8, rest)
    | IDENT "bytes" :: rest ->
        (* log "explicit rest"; *)
        (Rest, rest)
    | NUMBER n :: rest ->
        (* log "size is fixed bits %d" n; *)
        (Fixed_bits n, rest)
    | tokens ->
        failwith ~loc
          (Format.asprintf
             {|%s

Valid sizes are:

  %s       - match on the entire string
  %s - match `expr` bytes
  %s        - match on 1 UTF-8 grapheme
  %s  - match `expr` UTF-8 graphemes
  %s  - use up to `expr` bits
  %s    - use exactly `<number>` bits

For example:

  hello::bytes       – use all of `hello` as a byte string
  hello::bytes(len)  – use `len` bytes from `hello`
  hello::bits(len)   – use len bits of `hello` (128 bytes)
  hello::utf8        – use 1 valid utf8 grapheme from `hello`
  hello::7           – use 8 bits of `hello`
|}
             (error "Invalid size %S for %S" (Lexer.to_string tokens) name)
             (keyword "%s" "bytes")
             (keyword "%s" "bytes(expr)")
             (keyword "%s" "utf8")
             (keyword "%s" "utf8(expr)")
             (keyword "%s" "bits(expr)")
             (keyword "%s" "<number>"))

  and expect_comma ~loc:_ rest =
    match rest with COMMA :: rest -> `cont rest | _ -> `halt
end

let parse = Parser.parse

module Construction_lower = struct
  type t =
    | Empty
    | Bypass of string
    | Create_transient of string
    | Commit_transient of string
    | Add_rest of { src : string }
    | Add_next_utf8 of { src : string }
    | Add_next_fixed_bits of { src : string; size : int }
    | Add_next_dynamic_bits of { src : string; expr : Parsetree.expression }
    | Add_next_dynamic_bytes of { src : string; expr : Parsetree.expression }
    | Add_next_dynamic_utf8 of { src : string; expr : Parsetree.expression }
    | Add_int_fixed_bits of { value : int; size : int }
    | Add_int_dynamic_bits of { value : int; expr : Parsetree.expression }
    | Add_int_dynamic_bytes of { value : int; expr : Parsetree.expression }
    | Add_string_utf8 of { value : string }
    | Add_string_bytes of { value : string }
    | Add_string_dynamic_bytes of {
        value : string;
        expr : Parsetree.expression;
      }
    | Add_string_dynamic_utf8 of { value : string; expr : Parsetree.expression }

  let pp_one fmt t =
    match t with
    | Empty -> Format.fprintf fmt "Empty"
    | Bypass string -> Format.fprintf fmt "Bypass(%S)" string
    | Create_transient string ->
        Format.fprintf fmt "Create_transient(%S)" string
    | Commit_transient string ->
        Format.fprintf fmt "Commit_transient(%S)" string
    | Add_rest { src } -> Format.fprintf fmt "Add_rest {src=%S}" src
    | Add_next_utf8 { src } -> Format.fprintf fmt "Add_next_utf8 {src=%S)" src
    | Add_next_fixed_bits { src; size } ->
        Format.fprintf fmt "Add_next_fixed_bits {src=%S; size=%d}" src size
    | Add_next_dynamic_bits { src; expr } ->
        Format.fprintf fmt "Add_next_dynamic_bits {src=%S; expr=%S}" src
          (Pprintast.string_of_expression expr)
    | Add_next_dynamic_bytes { src; expr } ->
        Format.fprintf fmt "Add_next_dynamic_bytes {src=%S; expr=%S}" src
          (Pprintast.string_of_expression expr)
    | Add_next_dynamic_utf8 { src; expr } ->
        Format.fprintf fmt "Add_next_dynamic_utf8 {src=%s; expr=%S}" src
          (Pprintast.string_of_expression expr)
    | Add_int_fixed_bits { value; size } ->
        Format.fprintf fmt "Add_int_fixed_bits {value=%d; size=%d}" value size
    | Add_int_dynamic_bits { value; expr } ->
        Format.fprintf fmt "Add_int_dynamic_bits {value=%d; size=%S}" value
          (Pprintast.string_of_expression expr)
    | Add_int_dynamic_bytes { value; expr } ->
        Format.fprintf fmt "Add_int_dynamic_bytes {value=%d; size=%S}" value
          (Pprintast.string_of_expression expr)
    | Add_string_utf8 { value } ->
        Format.fprintf fmt "Add_string_utf8 {value=%S}" value
    | Add_string_bytes { value } ->
        Format.fprintf fmt "Add_string_bytes {value=%S}" value
    | Add_string_dynamic_bytes { value; expr } ->
        Format.fprintf fmt "Add_string_dynamic_bytes {value=%S; expr=%S}" value
          (Pprintast.string_of_expression expr)
    | Add_string_dynamic_utf8 { value; expr } ->
        Format.fprintf fmt "Add_string_dynamic_utf8 {value=%S; expr=%S}" value
          (Pprintast.string_of_expression expr)

  let pp fmt t =
    Format.fprintf fmt "[\r\n  ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\r\n  ")
      pp_one fmt t;
    Format.fprintf fmt "\r\n]\r\n"

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let rec lower ~loc patterns =
    match patterns with
    | [] -> [ Empty ]
    | [ Bind { name; size = Rest } ] -> [ Bypass name ]
    | pats ->
        let trns = "_trns" in
        [ Create_transient trns ] @ create_ops ~loc pats []
        @ [ Commit_transient trns ]

  and create_ops ~loc pats acc =
    match pats with
    | [] -> List.rev acc
    | Bind { name = src; size = Rest } :: rest ->
        (* log "add_rest %s\n" src; *)
        create_ops ~loc rest (Add_rest { src } :: acc)
    | Bind { name = src; size = Utf8 } :: rest ->
        (* log "add_next_utf8 %s\n" src; *)
        create_ops ~loc rest (Add_next_utf8 { src } :: acc)
    | Bind { name = src; size = Fixed_bits size } :: rest ->
        (* log "add_next_fixe_bits %s %d\n" src size; *)
        create_ops ~loc rest (Add_next_fixed_bits { src; size } :: acc)
    | Bind { name = src; size = Dynamic_bits expr } :: rest ->
        (* log "add_dynamic_bits %s %s\n" src expr; *)
        create_ops ~loc rest (Add_next_dynamic_bits { src; expr } :: acc)
    | Bind { name = src; size = Dynamic_utf8 expr } :: rest ->
        (* log "add_dynamic_utf8 %s %s\n" src expr; *)
        create_ops ~loc rest (Add_next_dynamic_utf8 { src; expr } :: acc)
    | Bind { name = src; size = Dynamic_bytes expr } :: rest ->
        (* log "add_dynamic_bytes %s %s\n" src expr; *)
        create_ops ~loc rest (Add_next_dynamic_bytes { src; expr } :: acc)
    (*
      handle number literals
    *)
    | Expect
        { value = Number value; size = (Rest | Utf8 | Dynamic_utf8 _) as size }
      :: _rest ->
        let[@warning "-8"] size =
          match size with
          | Rest -> "bytes"
          | Utf8 -> "utf8"
          | Dynamic_utf8 n -> "utf8(" ^ Pprintast.string_of_expression n ^ ")"
        in
        failwith ~loc
          (Format.sprintf
             {|%s

Valid sizes for number literals are:

  %s - use up to `expr` bytes
  %s  - use up to `expr` bits
  %s    - use exactly `<number>` bits

For example:

  2112::bytes(10)  – use `2112` as a 10 byte integer
  2112::bits(len)  – use `2112` as a `len` bits integer
  2112::7          – use `2112` as a 7 bit integer
|}
             (error "Invalid size %S for value %d" size value)
             (keyword "%s" "bytes(expr)")
             (keyword "%s" "bits(expr)")
             (keyword "%s" "<number>"))
    | Expect { value = Number value; size = Dynamic_bits expr } :: rest ->
        create_ops ~loc rest (Add_int_dynamic_bits { value; expr } :: acc)
    | Expect { value = Number value; size = Dynamic_bytes expr } :: rest ->
        create_ops ~loc rest (Add_int_dynamic_bytes { value; expr } :: acc)
    | Expect { value = Number value; size = Fixed_bits size } :: rest ->
        create_ops ~loc rest (Add_int_fixed_bits { value; size } :: acc)
    (*
      handle string literals
    *)
    | Expect
        { value = String value; size = (Fixed_bits _ | Dynamic_bits _) as size }
      :: _rest ->
        let[@warning "-8"] size =
          match size with
          | Fixed_bits n -> string_of_int n
          | Dynamic_bits n -> "bits(" ^ Pprintast.string_of_expression n ^ ")"
        in
        failwith ~loc
          (Format.sprintf
             {|%s

Valid sizes for string literals are:

  %s       - match on the entire string
  %s - match `expr` bytes
  %s        - match on 1 UTF-8 grapheme
  %s  - match `expr` UTF-8 graphemes

For example:

  "rush"::bytes      – use all of the "rush" string
  "rush"::bytes(10)  – use "rush" as a 10-byte string
  "rush"::utf8       – use "rush" as a valid UTF-8 string
  "rush"::utf8(10)   – use "rush" as a 10-grapheme utf-8 string
|}
             (error "Invalid size %S for value %S" size value)
             (keyword "%s" "bytes")
             (keyword "%s" "bytes(expr)")
             (keyword "%s" "utf8")
             (keyword "%s" "utf8(expr)"))
    | Expect { value = String value; size = Utf8 } :: rest ->
        create_ops ~loc rest (Add_string_utf8 { value } :: acc)
    | Expect { value = String value; size = Rest } :: rest ->
        create_ops ~loc rest (Add_string_bytes { value } :: acc)
    | Expect { value = String value; size = Dynamic_bytes expr } :: rest ->
        create_ops ~loc rest (Add_string_dynamic_bytes { value; expr } :: acc)
    | Expect { value = String value; size = Dynamic_utf8 expr } :: rest ->
        create_ops ~loc rest (Add_string_dynamic_utf8 { value; expr } :: acc)
end

module Transient_builder = struct
  open Ppxlib
  open Ast_helper

  let id ~loc name =
    let longident = { loc; txt = Longident.parse name } in
    Exp.ident ~loc longident

  let var ~loc name = Ast_helper.Pat.var { loc; txt = name }

  let rec to_expr ~loc (lower : Construction_lower.t list) =
    match lower with
    | [ Empty ] -> [%expr Bytestring.empty]
    | [ Bypass name ] -> [%expr [%e id ~loc name]]
    | Create_transient name :: rest ->
        let trns_name = var ~loc name in
        [%expr
          let [%p trns_name] = Bytestring.Transient.create () in
          [%e to_expr ~loc rest]]
    | Commit_transient name :: [] ->
        [%expr Bytestring.Transient.commit [%e id ~loc name]]
    | Commit_transient _name :: _rest ->
        bug ~loc "commit transient must be the last action we translate"
    | Add_rest { src } :: rest ->
        [%expr
          Bytestring.Transient.add_string _trns [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_next_utf8 { src } :: rest ->
        [%expr
          Bytestring.Transient.add_utf8 _trns [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_next_fixed_bits { src; size } :: rest ->
        [%expr
          Bytestring.Transient.add_bits _trns
            ~size:[%e Exp.constant (Const.int size)]
            [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_next_dynamic_bits { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_bits _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_next_dynamic_bytes { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_string _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_next_dynamic_utf8 { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_utf8 _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Add_int_dynamic_bytes { value; expr } :: rest ->
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns
            ~size:([%e expr] * 8)
            [%e value];
          [%e to_expr ~loc rest]]
    | Add_int_dynamic_bits { value; expr } :: rest ->
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns ~size:[%e expr] [%e value];
          [%e to_expr ~loc rest]]
    | Add_int_fixed_bits { value; size } :: rest ->
        let size = Exp.constant (Const.int size) in
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns ~size:[%e size] [%e value];
          [%e to_expr ~loc rest]]
    | Add_string_dynamic_utf8 { value; expr } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_utf8 _trns ~size:[%e expr] [%e value];
          [%e to_expr ~loc rest]]
    | Add_string_dynamic_bytes { value; expr } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_string _trns ~size:[%e expr]
            [%e value];
          [%e to_expr ~loc rest]]
    | Add_string_utf8 { value } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_utf8 _trns [%e value];
          [%e to_expr ~loc rest]]
    | Add_string_bytes { value } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_string _trns [%e value];
          [%e to_expr ~loc rest]]
    | _ -> bug ~loc "invalid lower repr when cosntructing bytestring"

  let to_transient_builder ~loc patterns =
    let lower = Construction_lower.lower patterns in
    to_expr ~loc (lower ~loc)
end

let to_transient_builder = Transient_builder.to_transient_builder

module Matching_lower = struct
  type t =
    | Empty of string
    | Bypass of { src : string; name : string }
    | Create_iterator of string
    | Bind_rest of { iter : string; src : string }
    | Bind_next_utf8 of { iter : string; src : string }
    | Bind_next_fixed_bits of { iter : string; src : string; size : int }
    | Bind_next_dynamic_bits of {
        iter : string;
        src : string;
        expr : Parsetree.expression;
      }
    | Bind_next_dynamic_bytes of {
        iter : string;
        src : string;
        expr : Parsetree.expression;
      }
    | Bind_next_dynamic_utf8 of {
        iter : string;
        src : string;
        expr : Parsetree.expression;
      }
    | Expect_int_fixed_bits of { iter : string; value : int; size : int }
    | Expect_int_dynamic_bits of {
        iter : string;
        value : int;
        expr : Parsetree.expression;
      }
    | Expect_int_dynamic_bytes of {
        iter : string;
        value : int;
        expr : Parsetree.expression;
      }
    | Expect_string_utf8 of { iter : string; value : string }
    | Expect_string_bytes of { iter : string; value : string }
    | Expect_string_dynamic_bytes of {
        iter : string;
        value : string;
        expr : Parsetree.expression;
      }
    | Expect_string_dynamic_utf8 of {
        iter : string;
        value : string;
        expr : Parsetree.expression;
      }

  let pp_one fmt t =
    match t with
    | Empty src -> Format.fprintf fmt "Empty(%S)" src
    | Bypass { src; name } ->
        Format.fprintf fmt "Bypass {src=%S;name=%S}" src name
    | Create_iterator string -> Format.fprintf fmt "Create_iterator(%S)" string
    | Bind_rest { iter; src } ->
        Format.fprintf fmt "Bind_rest {src=%S; iter=%S}" src iter
    | Bind_next_utf8 { iter; src } ->
        Format.fprintf fmt "Bind_next_utf8 {src=%S; iter=%S}" src iter
    | Bind_next_fixed_bits { iter; src; size } ->
        Format.fprintf fmt "Bind_next_fixed_bits {src=%S; size=%d; iter=%S}" src
          size iter
    | Bind_next_dynamic_bits { iter; src; expr } ->
        Format.fprintf fmt "Bind_next_dynamic_bits {src=%S; expr=%S; iter=%S}"
          src
          (Pprintast.string_of_expression expr)
          iter
    | Bind_next_dynamic_bytes { iter; src; expr } ->
        Format.fprintf fmt "Bind_next_dynamic_bytes {src=%S; expr=%S; iter=%S}"
          src
          (Pprintast.string_of_expression expr)
          iter
    | Bind_next_dynamic_utf8 { iter; src; expr } ->
        Format.fprintf fmt "Bind_next_dynamic_utf8 {src=%S; expr=%S; iter=%S}"
          src
          (Pprintast.string_of_expression expr)
          iter
    | Expect_int_fixed_bits { iter; value; size } ->
        Format.fprintf fmt "Expect_int_fixed_bits {value=%d; size=%d; iter=%S}"
          value size iter
    | Expect_int_dynamic_bits { iter; value; expr } ->
        Format.fprintf fmt
          "Expect_int_dynamic_bits {value=%d; expr=%S; iter=%S}" value
          (Pprintast.string_of_expression expr)
          iter
    | Expect_int_dynamic_bytes { iter; value; expr } ->
        Format.fprintf fmt
          "Expect_int_dynamic_bytes {value=%d; expr=%S; iter=%S}" value
          (Pprintast.string_of_expression expr)
          iter
    | Expect_string_utf8 { iter; value } ->
        Format.fprintf fmt "Expect_string_utf8 {value=%S; iter=%S}" value iter
    | Expect_string_bytes { iter; value } ->
        Format.fprintf fmt "Expect_string_bytes {value=%S; iter=%S}" value iter
    | Expect_string_dynamic_bytes { iter; value; expr } ->
        Format.fprintf fmt
          "Expect_string_dynamic_bytes {value=%S; expr=%S; iter=%S}" value
          (Pprintast.string_of_expression expr)
          iter
    | Expect_string_dynamic_utf8 { iter; value; expr } ->
        Format.fprintf fmt
          "Expect_string_dynamic_utf8 {value=%S; expr=%S; iter=%S}" value
          (Pprintast.string_of_expression expr)
          iter

  let pp fmt t =
    Format.fprintf fmt "[\r\n  ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\r\n  ")
      pp_one fmt t;
    Format.fprintf fmt "\r\n]\r\n"

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let rec lower ~loc patterns =
    let iter = "_data_src" in
    match patterns with
    | [] -> [ Empty iter ]
    | [ Bind { name; size = Rest } ] -> [ Bypass { src = iter; name } ]
    | pats ->
        [ Create_iterator iter ]
        @ create_ops ~loc ~iter pats []
        @ [ Empty iter ]

  and create_ops ~loc ~iter pats acc =
    match pats with
    | [] -> List.rev acc
    | Bind { name = src; size = Rest } :: rest ->
        (* log "bind_rest %s\n" src; *)
        create_ops ~loc ~iter rest (Bind_rest { iter; src } :: acc)
    | Bind { name = src; size = Utf8 } :: rest ->
        (* log "bind_next_utf8 %s\n" src; *)
        create_ops ~loc ~iter rest (Bind_next_utf8 { iter; src } :: acc)
    | Bind { name = src; size = Fixed_bits size } :: rest ->
        (* log "bind_next_fixe_bits %s %d\n" src size; *)
        create_ops ~loc ~iter rest
          (Bind_next_fixed_bits { iter; src; size } :: acc)
    | Bind { name = src; size = Dynamic_bits expr } :: rest ->
        (* log "bind_dynamic_bits %s %s\n" src expr; *)
        create_ops ~loc ~iter rest
          (Bind_next_dynamic_bits { iter; src; expr } :: acc)
    | Bind { name = src; size = Dynamic_utf8 expr } :: rest ->
        (* log "bind_dynamic_utf8 %s %s\n" src expr; *)
        create_ops ~loc ~iter rest
          (Bind_next_dynamic_utf8 { iter; src; expr } :: acc)
    | Bind { name = src; size = Dynamic_bytes expr } :: rest ->
        (* log "bind_dynamic_bytes %s %s\n" src expr; *)
        create_ops ~loc ~iter rest
          (Bind_next_dynamic_bytes { iter; src; expr } :: acc)
    (*
      handle number literals
    *)
    | Expect
        { value = Number value; size = (Utf8 | Dynamic_utf8 _ | Rest) as size }
      :: _rest ->
        let[@warning "-8"] size =
          match size with
          | Utf8 -> "utf8"
          | Rest -> "bytes"
          | Dynamic_utf8 n -> "utf8(" ^ Pprintast.string_of_expression n ^ ")"
        in
        failwith ~loc
          (Format.sprintf
             {|%s

Valid sizes for number literals are:

  %s - use up to `expr` bytes
  %s  - use up to `expr` bits
  %s    - use exactly `<number>` bits

For example:

  2112::bytes(10)  – use `2112` as a 10 byte integer
  2112::bits(len)  – use `2112` as a `len` bits integer
  2112::7          – use `2112` as a 7 bit integer
|}
             (error "Invalid size %S for expected number %d" size value)
             (keyword "%s" "bytes(expr)")
             (keyword "%s" "bits(expr)")
             (keyword "%s" "<number>"))
    | Expect { value = Number value; size = Dynamic_bits expr } :: rest ->
        create_ops ~loc ~iter rest
          (Expect_int_dynamic_bits { iter; value; expr } :: acc)
    | Expect { value = Number value; size = Dynamic_bytes expr } :: rest ->
        create_ops ~loc ~iter rest
          (Expect_int_dynamic_bytes { iter; value; expr } :: acc)
    | Expect { value = Number value; size = Fixed_bits size } :: rest ->
        create_ops ~loc ~iter rest
          (Expect_int_fixed_bits { iter; value; size } :: acc)
    (*
      handle string literals
    *)
    | Expect
        { value = String value; size = (Fixed_bits _ | Dynamic_bits _) as size }
      :: _rest ->
        let[@warning "-8"] size =
          match size with
          | Fixed_bits n -> string_of_int n
          | Dynamic_bits n -> "bits(" ^ Pprintast.string_of_expression n ^ ")"
        in
        failwith ~loc
          (Format.sprintf
             {|%s

Valid sizes for string literals are:

  %s       - match on the entire string
  %s - match `expr` bytes
  %s        - match on 1 UTF-8 grapheme
  %s  - match `expr` UTF-8 graphemes

For example:

  "rush"::bytes      – use all of the "rush" string
  "rush"::bytes(10)  – use "rush" as a 10-byte string
  "rush"::utf8       – use "rush" as a valid UTF-8 string
  "rush"::utf8(10)   – use "rush" as a 10-grapheme utf-8 string
|}
             (error "Invalid size %S for expected string %S" size value)
             (keyword "%s" "bytes")
             (keyword "%s" "bytes(expr)")
             (keyword "%s" "utf8")
             (keyword "%s" "utf8(expr)"))
    | Expect { value = String value; size = Utf8 } :: rest ->
        create_ops ~loc ~iter rest (Expect_string_utf8 { iter; value } :: acc)
    | Expect { value = String value; size = Rest } :: rest ->
        create_ops ~loc ~iter rest (Expect_string_bytes { iter; value } :: acc)
    | Expect { value = String value; size = Dynamic_bytes expr } :: rest ->
        create_ops ~loc ~iter rest
          (Expect_string_dynamic_bytes { iter; value; expr } :: acc)
    | Expect { value = String value; size = Dynamic_utf8 expr } :: rest ->
        create_ops ~loc ~iter rest
          (Expect_string_dynamic_utf8 { iter; value; expr } :: acc)
end

module Pattern_matcher = struct
  open Ppxlib
  open Ast_helper

  let id ~loc name =
    let longident = { loc; txt = Longident.parse name } in
    Exp.ident ~loc longident

  let var ~loc name = Ast_helper.Pat.var { loc; txt = name }

  let rec to_expr ~loc ~body (lower : Matching_lower.t list) =
    match lower with
    | [] -> body
    | [ Empty src ] ->
        [%expr
          Bytestring.Iter.expect_empty [%e id ~loc src];
          [%e body]]
    | [ Bypass { src; name } ] ->
        [%expr
          let [%p var ~loc name] = [%e id ~loc src] in
          [%e body]]
    | Create_iterator name :: rest ->
        let src = id ~loc name in
        let iter_name = var ~loc name in
        [%expr
          let [%p iter_name] = Bytestring.to_iter [%e src] in
          [%e to_expr ~body ~loc rest]]
    | Bind_rest { src; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] = Bytestring.Iter.rest [%e iter] in
          [%e to_expr ~body ~loc rest]]
    | Bind_next_utf8 { src; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] = Bytestring.Iter.next_utf8 [%e iter] in
          [%e to_expr ~body ~loc rest]]
    | Bind_next_fixed_bits { src; size; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] =
            Bytestring.Iter.next_bits
              ~size:[%e Exp.constant (Const.int size)]
              [%e iter]
          in
          [%e to_expr ~body ~loc rest]]
    | Bind_next_dynamic_bits { src; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] = Bytestring.Iter.next_bits ~size:[%e expr] [%e iter] in
          [%e to_expr ~body ~loc rest]]
    | Bind_next_dynamic_bytes { src; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] = Bytestring.Iter.next_bytes ~size:[%e expr] [%e iter] in
          [%e to_expr ~body ~loc rest]]
    | Bind_next_dynamic_utf8 { src; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let src = var ~loc src in
        [%expr
          let [%p src] = Bytestring.Iter.next_utd8 ~size:[%e expr] [%e iter] in
          [%e to_expr ~body ~loc rest]]
    | Expect_int_dynamic_bytes { value; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Iter.expect_literal_int [%e iter]
            ~size:([%e expr] * 8)
            [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_int_dynamic_bits { value; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Iter.expect_literal_int [%e iter] ~size:[%e expr]
            [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_int_fixed_bits { value; size; iter } :: rest ->
        let iter = id ~loc iter in
        let size = Exp.constant (Const.int size) in
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Iter.expect_literal_int [%e iter] ~size:[%e size]
            [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_string_dynamic_utf8 { value; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Iter.expect_literal_utf8 [%e iter] ~size:[%e expr]
            [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_string_dynamic_bytes { value; expr; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Iter.expect_literal_string [%e iter] ~size:[%e expr]
            [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_string_utf8 { value; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Iter.expect_literal_utf8 [%e iter] [%e value];
          [%e to_expr ~body ~loc rest]]
    | Expect_string_bytes { value; iter } :: rest ->
        let iter = id ~loc iter in
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Iter.expect_literal_string [%e iter] [%e value];
          [%e to_expr ~body ~loc rest]]
    | rest ->
        Location.raise_errorf ~loc "invalid lower repr: %a" Matching_lower.pp
          rest

  let to_pattern_match ~loc ~body patterns =
    let lower = Matching_lower.lower patterns in
    to_expr ~body ~loc (lower ~loc)
end

module Prefix_matching = struct
  type t =
    | Try_run of
        Matching_lower.t list
        * (Parsetree.expression option * Parsetree.expression)
    | Prefix of Matching_lower.t list * t list

  let rec pp fmt t =
    Format.fprintf fmt "[\r\n  ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\r\n  ")
      pp_one fmt t;
    Format.fprintf fmt "\r\n]\r\n"

  and pp_one fmt t =
    match t with
    | Prefix (ops, t) ->
        Format.fprintf fmt "Prefix(%a, %a)" Matching_lower.pp ops pp t
    | Try_run (ops, (guard, body)) ->
        Format.fprintf fmt "Try_run(%a, (%s, id %S))" Matching_lower.pp ops
          (match guard with
          | None -> "None"
          | Some guard ->
              Format.sprintf "Some (ocaml %S)"
                (Pprintast.string_of_expression guard))
          (Pprintast.string_of_expression body)

  let rec prefix op1 op2 =
    match (op1, op2) with
    | el1 :: rest1, el2 :: rest2 when el1 = el2 ->
        let prefix, rest1, rest2 = prefix rest1 rest2 in
        (el1 :: prefix, rest1, rest2)
    | _ -> ([], op1, op2)

  let merge p1 p2 =
    match (p1, p2) with
    | _, Prefix ([], []) ->
        (* log "skipped p2\n"; *)
        p1
    | Prefix ([], []), _ ->
        (* log "skipped p1\n"; *)
        p2
    | Prefix (op1, body1), Prefix (op2, body2) ->
        (* log "merging prefix with prefix\n"; *)
        let prefix, op1, op2 = prefix op1 op2 in
        Prefix (prefix, [ Prefix (op1, body1); Prefix (op2, body2) ])
    | Try_run (op1, body1), Try_run (op2, body2) ->
        (* log "merging try_run with try_run\n"; *)
        let prefix, op1, op2 = prefix op1 op2 in
        Prefix (prefix, [ Try_run (op1, body1); Try_run (op2, body2) ])
    | Prefix (op1, body1), Try_run (op2, body2) ->
        (* log "merging prefix with try_run\n"; *)
        let prefix, op1, op2 = prefix op1 op2 in
        Prefix (prefix, [ Prefix (op1, body1); Try_run (op2, body2) ])
    | Try_run (op1, body1), Prefix (op2, body2) ->
        (* log "merging try_run with prefix\n"; *)
        let prefix, op1, op2 = prefix op1 op2 in
        Prefix (prefix, [ Try_run (op1, body1); Prefix (op2, body2) ])

  let rec compact t =
    match t with
    | Prefix (ops, ts) ->
        let ts =
          List.concat_map
            (fun t ->
              match t with
              | Prefix ([], ts2) -> List.map compact ts2
              | t -> [ compact t ])
            ts
        in
        Prefix (ops, ts)
    | _ -> t

  let to_prefix_match ~loc patterns =
    let cases =
      List.map
        (fun (pattern, guard, body) ->
          Try_run (Matching_lower.lower ~loc pattern, (guard, body)))
        patterns
    in
    List.fold_left merge (Prefix ([], [])) cases |> compact

  let rec to_expr ~loc t =
    match t with
    | Prefix ([], t) -> build_body ~loc t
    | Prefix (ops, t) ->
        let body = build_body ~loc t in
        Pattern_matcher.to_expr ~loc ~body ops
    | Try_run (ops, (guard, body)) ->
        let body =
          match guard with
          | None -> body
          | Some guard ->
              [%expr
                if [%e guard] then [%e body]
                else raise Bytestring.Guard_mismatch]
        in
        Pattern_matcher.to_expr ~loc ~body ops

  and build_body ~loc t =
    if is_valid_guarded_leafs t then build_ifelse ~loc t
    else
      List.fold_left
        (fun acc t ->
          let case = to_expr ~loc t in
          [%expr try [%e case] with Bytestring.No_match -> [%e acc]])
        [%expr raise Bytestring.No_match] (List.rev t)

  and is_valid_guarded_leafs t =
    match t with
    | Try_run ([], (_, _body)) :: [] -> true
    | Try_run ([], (Some _, _body)) :: t -> true && is_valid_guarded_leafs t
    | _ -> false

  and[@warning "-8"] build_ifelse ~loc t =
    let bodies, last =
      match List.rev t with
      | Try_run ([], (None, body)) :: bodies -> (bodies, body)
      | Try_run ([], (Some _guard, _)) :: _ as bodies ->
          (bodies, [%expr raise Bytestring.Guard_mismatch])
      | _ -> bug ~loc "build_ifelse should have been validated"
    in
    List.fold_left
      (fun acc (Try_run ([], (Some guard, body))) ->
        [%expr if [%e guard] then [%e body] else [%e acc]])
      last bodies

  let to_match_expression ~loc ~data patterns =
    let expr = to_expr ~loc (to_prefix_match ~loc patterns) in
    [%expr (fun _data_src -> [%e expr]) [%e data]]
end

let to_pattern_match = Pattern_matcher.to_pattern_match
let to_prefix_match = Prefix_matching.to_prefix_match
let to_match_expression = Prefix_matching.to_match_expression
