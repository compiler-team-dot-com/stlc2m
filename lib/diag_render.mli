module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Diag : Diag.S with module Range = Ast.Range)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) : sig
  val render : Ast_index.t -> Diag_core.t -> Diag.t
end
