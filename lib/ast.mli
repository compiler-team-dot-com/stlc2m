module type Range = sig
  type t = { start_pos : Lexing.position; end_pos : Lexing.position }

  val string_of_position : Lexing.position -> string
  val to_string : t -> string
end

module type S = sig
  module Range : Range

  type range = Range.t
  type node_id
  type ty = TInt | TBool | TArrow of ty * ty

  type expr = { id : node_id; node : expr_node; range : range }

  and expr_node =
    | EVar of string
    | EInt of int
    | EBool of bool
    | EIf of expr * expr * expr
    | ELam of string * range * ty * expr (* fun (x : ty) -> body *)
    | EApp of expr * expr
    | ELet of string * range * expr * expr (* let x = e1 in e2 *)
    | ELetStack of {
        kw_range : range;
        x : string;
        x_range : range;
        e1 : expr;
        e2 : expr;
      }
    | EExport of expr (* export e *)

  val string_of_ty : ty -> string
  val string_of_node_id : node_id -> string
  val mk_expr : expr_node -> node_id -> range -> expr

  module IdMap : Map.S with type key = node_id
end

module Make (Id : Id_gen.Id) : S with type node_id = Id.t
