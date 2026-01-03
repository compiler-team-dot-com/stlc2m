module type S = Checker_report_intf.S

module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Diag : Diag.S with module Range = Ast.Range)
    (Checker :
      Checker.S
        with type expr = Ast.expr
         and type ty = Ast.ty
         and type range = Ast.Range.t
         and type node_id = Ast.node_id) :
  S
    with type ast_index = Ast_index.t
     and type error = Checker.error
     and type diag = Diag.t
