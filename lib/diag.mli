module type S = Diag_intf.S

module Make (Ast : Ast.S) : S with module Range = Ast.Range
