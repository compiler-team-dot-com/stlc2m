module Make
    (Ast : Ast.S)
    (Checker :
      Checker.S with type expr = Ast.expr and type node_id = Ast.node_id)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) : sig
  type kind = Action_kind.t

  type proposal = {
    kind : kind;
    title : string;
    targets : Ast.node_id list;
    rationale : string;
    apply : Ast.expr -> Ast.expr option; (* None for Explain *)
  }

  val propose : Diag_core.t -> Checker.error -> proposal list
  val propose_global : root:Ast.expr -> proposal list
end
