open Ppxlib

let tag = "bytestring"

let mk_expression ~ctxt:_ expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (value, _, _)) ->
      let bytepattern = Bytepattern.parse value in
      Bytepattern.to_transient_builder ~loc bytepattern
  | Pexp_match (data, cases) ->
      let cases =
        List.map
          (fun (case : case) ->
            match case.pc_lhs with
            | { ppat_desc = Ppat_constant (Pconst_string (value, _, _)); _ } ->
                let bytepattern = Bytepattern.parse value in
                let body = case.pc_rhs in
                (bytepattern, body)
            | _ ->
                Location.raise_errorf ~loc "%s"
                  {| Bytestrings in match expressions are only valie when the patterns are constant strings, like:

  match%bytestring data with
  | \{| ... |\} -> ...

|})
          cases
      in

      Bytepattern.to_match_expression ~loc ~data cases
  | _ ->
      Location.raise_errorf ~loc "%s"
        {|Bytestrings are only supported when constructing new values with
tagged with `%bytestring` and when pattern matching against values in
`match` expressions.
|}

let expression_rule =
  Extension.V3.declare tag Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    mk_expression
  |> Context_free.Rule.extension

let () = Driver.register_transformation tag ~rules:[ expression_rule ]
