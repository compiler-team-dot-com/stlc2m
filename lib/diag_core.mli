module type S = Diag_core_intf.S

module Make (Ast : Ast.S) :
  S with type node_id = Ast.node_id and type ty = Ast.ty
