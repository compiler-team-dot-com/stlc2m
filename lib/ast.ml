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

module Make (Id : Id_gen.Id) = struct
  module Range : Range = struct
    type t = { start_pos : Lexing.position; end_pos : Lexing.position }

    let string_of_position (p : Lexing.position) : string =
      (* Lexing.position.pos_lnum is 1-based; pos_cnum and pos_bol allow column computation *)
      let line = p.pos_lnum in
      let col = p.pos_cnum - p.pos_bol in
      Printf.sprintf "%d:%d" line col

    let to_string r =
      Printf.sprintf "%s-%s"
        (string_of_position r.start_pos)
        (string_of_position r.end_pos)
  end

  type range = Range.t
  type node_id = Id.t
  type ty = TInt | TBool | TArrow of ty * ty

  module IdMap = Map.Make (struct
    type t = node_id

    let compare = Id.compare
  end)

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

  let mk_expr expr_node id range = { id; node = expr_node; range }

  (* let mk_range (start_pos : Lexing.position) (end_pos : Lexing.position) : range *)
  (*     = *)
  (*   { start_pos; end_pos } *)

  (* let mk_range_node (g : Id_gen.t) (sp : Lexing.position) (ep : Lexing.position) *)
  (* (\*     (node : expr_node) : expr = *\) *)
  (*   let id = Id_gen.fresh g in *)
  (*   let range = mk_range sp ep in *)
  (*   { id; node; range } *)

  (* prints arrows right-associatevely and parenthesizes left arrows *)
  let rec string_of_ty = function
    | TInt -> "Int"
    | TBool -> "Bool"
    | TArrow (t1, t2) ->
        let left =
          match t1 with
          | TArrow _ -> "(" ^ string_of_ty t1 ^ ")"
          | _ -> string_of_ty t1
        in
        left ^ " -> " ^ string_of_ty t2

  let string_of_node_id = Id.to_string
end
