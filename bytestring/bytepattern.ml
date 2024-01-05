type token =
  | IDENT of string
  | COMMA
  | COLON_COLON
  | EOF
  | NUMBER of int64
  | EXPRESSION of string

module Lexer = struct
  let digit = [%sedlex.regexp? Plus '0' .. '9']
  let letter = [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z']
  let ident = [%sedlex.regexp? letter, Star (letter | digit)]

  let rec token buf acc =
    match%sedlex buf with
    | white_space -> token buf acc
    | "," ->
        Printf.printf ",%!";
        token buf (COMMA :: acc)
    | "::" ->
        Printf.printf "::%!";
        token buf (COLON_COLON :: acc)
    | "(", Star white_space ->
        Printf.printf "(%!";
        expr buf acc
    | ident ->
        let ident = Sedlexing.Utf8.lexeme buf in
        Printf.printf "%s%!" ident;
        token buf (IDENT ident :: acc)
    | digit ->
        let num = Sedlexing.Utf8.lexeme buf in
        Printf.printf "%s%!" num;
        token buf (NUMBER (Int64.of_string num) :: acc)
    | eof ->
        Printf.printf "\n%!";
        acc
    | _ -> failwith "Unexpected character"

  and expr buf ?(exp = []) acc =
    match%sedlex buf with
    | Star white_space, ")" ->
        let expr = List.rev exp |> String.concat "" in
        Printf.printf "%s)%!" expr;
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
