open Ppxlib
(* let log = Printf.printf *)

type token =
  | IDENT of string
  | COMMA
  | COLON_COLON
  | EOF
  | NUMBER of int
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
    | NUMBER n :: COLON_COLON :: rest -> (
        (* log "expect %n:: " n; *)
        let value = Number n in
        let size, rest = parse_size rest in
        match expect_comma rest with
        | `cont rest -> (expect value size, rest)
        | `halt -> (expect value size, []))
    | NUMBER n :: rest ->
        (* log "expect %d with implied size of 1 byte" n; *)
        (expect (Number n) (Fixed_bits 8), rest)
    | _ -> failwith "patterns must begin with identifiers or constants"

  and parse_size (tokens : token list) =
    match tokens with
    | NUMBER n :: rest ->
        (* log "size is fixed bits %d" n; *)
        (Fixed_bits n, rest)
    | IDENT "bytes" :: EXPRESSION expr :: rest ->
        (* log "dynamic bytes(%s)" expr; *)
        (Dynamic_bytes expr, rest)
    | IDENT "size" :: EXPRESSION expr :: rest ->
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
    | _ -> failwith "invalid size"

  and expect_comma rest =
    match rest with COMMA :: rest -> `cont rest | _ -> `halt
end

let parse = Parser.parse

module Lower = struct
  type t =
    | Empty
    | Bypass of string
    | Create_iter of string
    | Create_transient of string
    | Commit_transient of string
    | Add_rest of { iter : string }
    | Add_next_utf8 of { iter : string }
    | Add_next_fixed_bits of { iter : string; size : int }
    | Add_next_dynamic_bits of { iter : string; expr : Parsetree.expression }
    | Add_next_dynamic_bytes of { iter : string; expr : Parsetree.expression }
    | Add_next_dynamic_utf8 of { iter : string; expr : Parsetree.expression }
    | Add_literal_int of { value : int }
    | Add_literal_string of { value : string }

  let pp_one fmt t =
    match t with
    | Empty -> Format.fprintf fmt "Empty"
    | Bypass string -> Format.fprintf fmt "Bypass(%S)" string
    | Create_iter string -> Format.fprintf fmt "Create_iter(%S)" string
    | Create_transient string ->
        Format.fprintf fmt "Create_transient(%S)" string
    | Commit_transient string ->
        Format.fprintf fmt "Commit_transient(%S)" string
    | Add_rest { iter } -> Format.fprintf fmt "Add_rest {iter=%S}" iter
    | Add_next_utf8 { iter } ->
        Format.fprintf fmt "Add_next_utf8 {iter=%S)" iter
    | Add_next_fixed_bits { iter; size } ->
        Format.fprintf fmt "Add_next_fixed_bits {iter=%S; size=%d}" iter size
    | Add_next_dynamic_bits { iter; expr } ->
        Format.fprintf fmt "Add_next_dynamic_bits {iter=%s; expr=%S}" iter
          (Pprintast.string_of_expression expr)
    | Add_next_dynamic_bytes { iter; expr } ->
        Format.fprintf fmt "Add_next_dynamic_bytes {iter=%s; expr=%S}" iter
          (Pprintast.string_of_expression expr)
    | Add_next_dynamic_utf8 { iter; expr } ->
        Format.fprintf fmt "Add_next_dynamic_utf8 {iter=%s; expr=%S}" iter
          (Pprintast.string_of_expression expr)
    | Add_literal_int { value } ->
        Format.fprintf fmt "Add_literal_int {value=%d}" value
    | Add_literal_string { value } ->
        Format.fprintf fmt "Add_literal_string {value=%S}" value

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
        let iterators = create_iterators pats [] in
        let create_transient, commit_transient =
          if List.length iterators > 0 then
            ([ Create_transient trns ], [ Commit_transient trns ])
          else ([], [])
        in
        create_transient @ iterators @ create_ops pats [] @ commit_transient

  and create_iterators pats acc =
    match pats with
    | [] -> Set.(of_list acc |> to_list)
    | Bind { name; _ } :: rest ->
        (* log "create_iter %s\n" name; *)
        let acc = Create_iter name :: acc in
        create_iterators rest acc
    | _ :: rest -> create_iterators rest acc

  and create_ops pats acc =
    match pats with
    | [] -> List.rev acc
    | Bind { name = iter; size = Rest } :: rest ->
        (* log "add_rest %s\n" iter; *)
        create_ops rest (Add_rest { iter } :: acc)
    | Bind { name = iter; size = Utf8 } :: rest ->
        (* log "add_next_utf8 %s\n" iter; *)
        create_ops rest (Add_next_utf8 { iter } :: acc)
    | Bind { name = iter; size = Fixed_bits size } :: rest ->
        (* log "add_next_fixe_bits %s %d\n" iter size; *)
        create_ops rest (Add_next_fixed_bits { iter; size } :: acc)
    | Bind { name = iter; size = Dynamic_bits expr } :: rest ->
        (* log "add_dynamic_bits %s %s\n" iter expr; *)
        create_ops rest (Add_next_dynamic_bits { iter; expr } :: acc)
    | Bind { name = iter; size = Dynamic_utf8 expr } :: rest ->
        (* log "add_dynamic_utf8 %s %s\n" iter expr; *)
        create_ops rest (Add_next_dynamic_utf8 { iter; expr } :: acc)
    | Bind { name = iter; size = Dynamic_bytes expr } :: rest ->
        (* log "add_dynamic_bytes %s %s\n" iter expr; *)
        create_ops rest (Add_next_dynamic_bytes { iter; expr } :: acc)
    | Expect { value = Number value; size } :: rest ->
        (* log "add_literal_int  %d\n" value; *)
        create_ops rest (Add_literal_int { value } :: acc)
    | Expect { value = String value; size } :: rest ->
        (* log "add_literal_string %s\n" value; *)
        create_ops rest (Add_literal_string { value } :: acc)
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
    | Lower.Create_iter name :: rest ->
        let iter_name = var ~loc ("_iter_" ^ name) in
        [%expr
          let [%p iter_name] = Bytestring.to_iter [%e id ~loc name] in
          [%e to_expr ~loc rest]]
    | Lower.Create_transient name :: rest ->
        let trns_name = var ~loc name in
        [%expr
          let [%p trns_name] = Bytestring.Transient.create () in
          [%e to_expr ~loc rest]]
    | Lower.Commit_transient name :: [] ->
        [%expr Bytestring.Transient.commit [%e id ~loc name]]
    | Lower.Commit_transient name :: _rest ->
        failwith "commit transient must be the last action we translate"
    | Lower.Add_rest { iter } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        [%expr
          Bytestring.Transient.add_string _trns (Bytestring.Iter.rest [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_next_utf8 { iter } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        [%expr
          Bytestring.Transient.add_string _trns
            (Bytestring.Iter.next_utf8 [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_next_fixed_bits { iter; size } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        [%expr
          Bytestring.Transient.add_string _trns
            (Bytestring.Iter.next_bits
               ~size:[%e Exp.constant (Const.int size)]
               [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_bits { iter; expr } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        let expr = ([%expr expr] [@subst let expr : string = expr]) in
        [%expr
          Bytestring.Transient.add_string _trns
            (Bytestring.Iter.next_bits ~size:[%e expr] [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_bytes { iter; expr } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        [%expr
          Bytestring.Transient.add_string _trns
            (Bytestring.Iter.next_bytes ~size:[%e expr] [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_next_dynamic_utf8 { iter; expr } :: rest ->
        let iter = id ~loc ("_iter_" ^ iter) in
        let expr = ([%expr e] [@subst let e : string = expr]) in
        [%expr
          Bytestring.Transient.add_string _trns
            (Bytestring.Iter.next_utf8_seq ~size:[%e expr] [%e iter]);
          [%e to_expr ~loc rest]]
    | Lower.Add_literal_int { value } :: rest ->
        [%expr
          Bytestring.Transient.add_literal_int _trns int;
          [%e to_expr ~loc rest]]
        [@subst let int : int = value]
    | Lower.Add_literal_string { value } :: rest ->
        let str = [%expr Exp.constant (Const.string value)] in
        [%expr
          Bytestring.Transient.add_literal_string _trns [%e str];
          [%e to_expr ~loc rest]]
    | _ -> failwith "invalid lower reprt"

  let to_transient_builder ~loc (lower : Lower.t list) = 
    [%expr begin [%e to_expr ~loc lower ] end ]
end

let to_transient_builder = Translator.to_transient_builder
