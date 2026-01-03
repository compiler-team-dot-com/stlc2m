module type S = sig
  (* Include module Ast so we can "open Ast" in the parser. *)
  module Ast : Ast.S

  type t

  val create : unit -> t

  val mk_range_node :
    t -> Lexing.position -> Lexing.position -> Ast.expr_node -> Ast.expr

  val mk_range : Lexing.position -> Lexing.position -> Ast.Range.t
end
