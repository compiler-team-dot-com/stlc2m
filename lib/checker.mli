module type S = Checker_intf.S

module Make (Ast : Ast.S) :
  S
    with type expr = Ast.expr
     and type ty = Ast.ty
     and type range = Ast.Range.t
     and type node_id = Ast.node_id
