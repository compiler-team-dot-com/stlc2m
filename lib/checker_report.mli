module type S = Checker_report_intf.S

module Make
    (Ast : Ast.S)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty)
    (Checker :
      Checker.S
        with type expr = Ast.expr
         and type ty = Ast.ty
         and type node_id = Ast.node_id) :
  S with type error = Checker.error and type diag_core = Diag_core.t
