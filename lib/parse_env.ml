module type S = Parse_env_intf.S

module Make (Ast : Ast.S) (Gen : Id_gen.S with type Node_id.t = Ast.node_id) =
struct
  module Ast = Ast

  type gen = Gen.t
  type t = { gen : gen }
  type expr_node = Ast.expr_node
  type expr = Ast.expr

  let create () = { gen = Gen.create () }
  let mk_range sp ep = { Ast.Range.start_pos = sp; end_pos = ep }

  let mk_range_node { gen } sp ep (node : expr_node) : expr =
    let id = Gen.fresh gen in
    Ast.mk_expr node id { Ast.Range.start_pos = sp; end_pos = ep }
end
