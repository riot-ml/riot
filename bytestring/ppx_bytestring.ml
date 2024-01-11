open Ppxlib

let tag = "b"

let mk_expression ~ctxt:_ expr =
  try
    let loc = expr.pexp_loc in
    match expr.pexp_desc with
    | Pexp_constant (Pconst_string (value, _, _)) ->
        let bytepattern = Bytepattern.parse ~loc value in
        Bytepattern.to_transient_builder ~loc bytepattern
    | Pexp_match (data, cases) ->
        let cases =
          List.map
            (fun (case : case) ->
              match case.pc_lhs with
              | { ppat_desc = Ppat_constant (Pconst_string (value, _, _)); _ }
                ->
                  let bytepattern = Bytepattern.parse ~loc value in
                  let body = case.pc_rhs in
                  let guard = case.pc_guard in
                  (bytepattern, guard, body)
              | _ ->
                  Location.raise_errorf ~loc "%s"
                    {| Bytestrings in match expressions are only valie when the patterns are constant strings, like:

  match%b data with
  | \{| ... |\} -> ...

|})
            cases
        in

        Bytepattern.to_match_expression ~loc ~data cases
    | _ ->
        Location.raise_errorf ~loc "%s"
          {|Bytestrings are only supported when constructing new values with
tagged with `%b and when pattern matching against values in
`match` expressions.
|}
  with Bytepattern.Error { loc; error } ->
    let ext = Location.error_extensionf ~loc "%s" error in
    Ast_builder.Default.pexp_extension ~loc ext

let expression_rule =
  Extension.V3.declare tag Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    mk_expression
  |> Context_free.Rule.extension

let () = Driver.register_transformation tag ~rules:[ expression_rule ]
