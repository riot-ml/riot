open Ppxlib
open Ast_builder.Default

let tag = "bin"

(*
let mk_expression ~ctxt expr = 
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (value, _, _)) -> ()

let expression_rule = Extension.V3.declare
  tag
  Extension.Context.expression
  Ast_pattern.(single_expr_payload __)
  mk_expression
  |> Context_free.Rule.extension

let () =
  Driver.register_transformation tag ~rules:[
    expression_rule
  ]
  *)
