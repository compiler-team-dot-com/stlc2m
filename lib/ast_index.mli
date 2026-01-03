module type S = Ast_index_intf.S

module Make (Ast : Ast.S) :
  S
    with type range = Ast.range
     and type expr = Ast.expr
     and type node_id = Ast.node_id
