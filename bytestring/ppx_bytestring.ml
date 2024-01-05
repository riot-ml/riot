open Ppxlib

let tag = "bytestring"

let mk_expression ~ctxt:_ expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (value, _, _)) ->
      let bytepattern = Bytepattern.(parse value |> lower) in
      Bytepattern.to_transient_builder ~loc bytepattern
  | _ -> expr

let expression_rule =
  Extension.V3.declare tag Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    mk_expression
  |> Context_free.Rule.extension

let () = Driver.register_transformation tag ~rules:[ expression_rule ]
