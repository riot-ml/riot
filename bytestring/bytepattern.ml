open Ppxlib
(* let log = Printf.printf *)

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

  let rec token buf acc =
    match%sedlex buf with
    | white_space -> token buf acc
    | "," ->
        (* log ",%!"; *)
        token buf (COMMA :: acc)
    | "::" ->
        (* log "::%!"; *)
        token buf (COLON_COLON :: acc)
    | "(", Star white_space ->
        (* log "(%!"; *)
        expr buf acc
    | "\"" ->
        (* log "\"%!"; *)
        string buf acc
    | ident ->
        let ident = Sedlexing.Utf8.lexeme buf in
        (* log "%s%!" ident; *)
        token buf (IDENT ident :: acc)
    | digit ->
        let num = Sedlexing.Utf8.lexeme buf in
        (* log "%s%!" num; *)
        token buf (NUMBER (Int64.of_string num |> Int64.to_int) :: acc)
    | eof ->
        (* log "\n%!"; *)
        acc
    | _ -> failwith "Unexpected character"

  and string buf ?(str = []) acc =
    match%sedlex buf with
    | "\"" ->
        let str = List.rev str |> String.concat "" in
        (* log "%s\"%!" str; *)
        token buf (STRING str :: acc)
    | any ->
        let ident = Sedlexing.Utf8.lexeme buf in
        string buf ~str:(ident :: str) acc
    | _ -> failwith "Unexpected character"

  and expr buf ?(exp = []) acc =
    match%sedlex buf with
    | Star white_space, ")" ->
        let expr = List.rev exp |> String.concat "" in
        (* log "%s)%!" expr; *)
        let lexbuf = Lexing.from_string ~with_positions:false expr in
        let expr = Parse.expression lexbuf in
        let expr = EXPRESSION expr in
        token buf (expr :: acc)
    | any ->
        let ident = Sedlexing.Utf8.lexeme buf in
        expr buf ~exp:(ident :: exp) acc
    | _ -> failwith "Unexpected character"

  let read str =
    let lexbuf = Sedlexing.Utf8.from_string str in
    token lexbuf [] |> List.rev
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

  let rec parse str =
    let tokens = Lexer.read str in
    if tokens = [] then [] else do_parse tokens []

  and do_parse tokens acc =
    match tokens with
    | [] -> List.rev acc
    | _ ->
        let pattern, rest = parse_pattern tokens in
        (* log "\n"; *)
        do_parse rest (pattern :: acc)

  and parse_pattern (tokens : token list) =
    match tokens with
    | IDENT name :: COLON_COLON :: rest -> (
        (* log "bind %s:: " name; *)
        let size, rest = parse_size rest in
        match expect_comma rest with
        | `cont rest -> (bind name size, rest)
        | `halt -> (bind name size, []))
    | IDENT name :: rest ->
        (* log "bind %s with implied rest" name; *)
        (bind name Rest, rest)
    | STRING s :: COLON_COLON :: rest -> (
        (* log "expect %n:: " n; *)
        let value = String s in
        let size, rest = parse_size rest in
        match expect_comma rest with
        | `cont rest -> (expect value size, rest)
        | `halt -> (expect value size, []))
    | STRING s :: rest ->
        (* log "expect %d with implied size of 1 byte" n; *)
        (expect (String s) (Fixed_bits 8), rest)
    | NUMBER n :: COLON_COLON :: rest -> (
        (* log "expect %n:: " n; *)
        let value = Number n in
        let size, rest = parse_size rest in
        match expect_comma rest with
        | `cont rest -> (expect value size, rest)
        | `halt -> (expect value size, []))
    | NUMBER n :: rest ->
        (* log "expect %d with implied size of 1 byte" n; *)
        (expect (Number n) (Fixed_bits 1), rest)
    | _ -> failwith "patterns must begin with identifiers or constants"

  and parse_size (tokens : token list) =
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
    | _ -> failwith "invalid size"

  and expect_comma rest =
    match rest with COMMA :: rest -> `cont rest | _ -> `halt
end

let parse = Parser.parse

module Lower = struct
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
        Format.fprintf fmt "Add_next_dynamic_bits {src=%s; expr=%S}" src
          (Pprintast.string_of_expression expr)
    | Add_next_dynamic_bytes { src; expr } ->
        Format.fprintf fmt "Add_next_dynamic_bytes {src=%s; expr=%S}" src
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

  let rec lower patterns =
    match patterns with
    | [] -> [ Empty ]
    | [ Bind { name; size = Rest } ] -> [ Bypass name ]
    | pats ->
        let trns = "_trns" in
        [ Create_transient trns ] @ create_ops pats []
        @ [ Commit_transient trns ]

  and create_ops pats acc =
    match pats with
    | [] -> List.rev acc
    | Bind { name = src; size = Rest } :: rest ->
        (* log "add_rest %s\n" src; *)
        create_ops rest (Add_rest { src } :: acc)
    | Bind { name = src; size = Utf8 } :: rest ->
        (* log "add_next_utf8 %s\n" src; *)
        create_ops rest (Add_next_utf8 { src } :: acc)
    | Bind { name = src; size = Fixed_bits size } :: rest ->
        (* log "add_next_fixe_bits %s %d\n" src size; *)
        create_ops rest (Add_next_fixed_bits { src; size } :: acc)
    | Bind { name = src; size = Dynamic_bits expr } :: rest ->
        (* log "add_dynamic_bits %s %s\n" src expr; *)
        create_ops rest (Add_next_dynamic_bits { src; expr } :: acc)
    | Bind { name = src; size = Dynamic_utf8 expr } :: rest ->
        (* log "add_dynamic_utf8 %s %s\n" src expr; *)
        create_ops rest (Add_next_dynamic_utf8 { src; expr } :: acc)
    | Bind { name = src; size = Dynamic_bytes expr } :: rest ->
        (* log "add_dynamic_bytes %s %s\n" src expr; *)
        create_ops rest (Add_next_dynamic_bytes { src; expr } :: acc)
    (*
      handle number literals
    *)
    | Expect { value = Number value; size = Utf8 | Dynamic_utf8 _ } :: _rest ->
        failwith
          (Format.sprintf
             "Invalid size `utf8` for number %d. UTF-8 sizes expect the value \
              to be a string literal."
             value)
    | Expect { value = Number value; size = Rest } :: _rest ->
        failwith
          (Format.sprintf
             "Invalid size `bytes` for number %d. The size `bytes` expects the \
              value to be a string literal."
             value)
    | Expect { value = Number value; size = Dynamic_bits expr } :: rest ->
        create_ops rest (Add_int_dynamic_bits { value; expr } :: acc)
    | Expect { value = Number value; size = Dynamic_bytes expr } :: rest ->
        create_ops rest (Add_int_dynamic_bytes { value; expr } :: acc)
    | Expect { value = Number value; size = Fixed_bits size } :: rest ->
        create_ops rest (Add_int_fixed_bits { value; size } :: acc)
    (*
      handle string literals
    *)
    | Expect { value = String value; size = Fixed_bits _ | Dynamic_bits _ }
      :: _rest ->
        failwith
          (Format.sprintf
             "Invalid string %S with size specified in bits. Bits sizes \
              expected the value to be an int."
             value)
    | Expect { value = String value; size = Utf8 } :: rest ->
        create_ops rest (Add_string_utf8 { value } :: acc)
    | Expect { value = String value; size = Rest } :: rest ->
        create_ops rest (Add_string_bytes { value } :: acc)
    | Expect { value = String value; size = Dynamic_bytes expr } :: rest ->
        create_ops rest (Add_string_dynamic_bytes { value; expr } :: acc)
    | Expect { value = String value; size = Dynamic_utf8 expr } :: rest ->
        create_ops rest (Add_string_dynamic_utf8 { value; expr } :: acc)
end

let lower = Lower.lower

module Translator = struct
  open Ppxlib
  open Ast_helper

  let id ~loc name =
    let longident = { loc; txt = Longident.parse name } in
    Exp.ident ~loc longident

  let var ~loc name = Ast_helper.Pat.var { loc; txt = name }

  let rec to_expr ~loc (lower : Lower.t list) =
    match lower with
    | [ Empty ] -> [%expr Bytestring.empty]
    | [ Lower.Bypass name ] -> [%expr [%e id ~loc name]]
    | Lower.Create_transient name :: rest ->
        let trns_name = var ~loc name in
        [%expr
          let [%p trns_name] = Bytestring.Transient.create () in
          [%e to_expr ~loc rest]]
    | Lower.Commit_transient name :: [] ->
        [%expr Bytestring.Transient.commit [%e id ~loc name]]
    | Lower.Commit_transient _name :: _rest ->
        failwith "commit transient must be the last action we translate"
    | Lower.Add_rest { src } :: rest ->
        [%expr
          Bytestring.Transient.add_string _trns [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_next_utf8 { src } :: rest ->
        [%expr
          Bytestring.Transient.add_utf8 _trns [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_next_fixed_bits { src; size } :: rest ->
        [%expr
          Bytestring.Transient.add_bits _trns
            ~size:[%e Exp.constant (Const.int size)]
            [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_bits { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_bits _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_bytes { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_string _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_utf8 { src; expr } :: rest ->
        [%expr
          Bytestring.Transient.add_utf8 _trns ~size:[%e expr] [%e id ~loc src];
          [%e to_expr ~loc rest]]
    | Lower.Add_int_dynamic_bytes { value; expr } :: rest ->
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns
            ~size:([%e expr] * 8)
            [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_int_dynamic_bits { value; expr } :: rest ->
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns ~size:[%e expr] [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_int_fixed_bits { value; size } :: rest ->
        let size = Exp.constant (Const.int size) in
        let value = Exp.constant (Const.int value) in
        [%expr
          Bytestring.Transient.add_literal_int _trns ~size:[%e size] [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_string_dynamic_utf8 { value; expr } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_utf8 _trns ~size:[%e expr] [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_string_dynamic_bytes { value; expr } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_string _trns ~size:[%e expr]
            [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_string_utf8 { value } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_utf8 _trns [%e value];
          [%e to_expr ~loc rest]]
    | Lower.Add_string_bytes { value } :: rest ->
        let value = Exp.constant (Const.string value) in
        [%expr
          Bytestring.Transient.add_literal_string _trns [%e value];
          [%e to_expr ~loc rest]]
    | _ -> failwith "invalid lower reprt"

  let to_transient_builder ~loc (lower : Lower.t list) =
    [%expr [%e to_expr ~loc lower]]
end

let to_transient_builder = Translator.to_transient_builder
