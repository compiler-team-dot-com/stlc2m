module type S = Parse_env_intf.S

module Make (Ast : Ast.S) (_ : Id_gen.S with type Node_id.t = Ast.node_id) :
  S with module Ast = Ast
