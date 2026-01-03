module type S = Ast_index_intf.S

module Make (Ast : Ast.S) = struct
  open Ast

  type range = Ast.range
  type expr = Ast.expr
  type node_id = Ast.node_id

  module NodeIdMap = Ast.IdMap

  let string_of_node_id = Ast.string_of_node_id

  type t = {
    node_of : expr NodeIdMap.t;
    parent_of : node_id option NodeIdMap.t;
  }

  (* Snapshot discipline: we make index lookup total by construction.
   If the server stores a coherent snapshot (AST and index) built together,
   then any node that is missing from the index is treated as an internal bug. *)
  exception Bug of string

  let empty_index : t =
    { node_of = NodeIdMap.empty; parent_of = NodeIdMap.empty }

  let add_parent (idx : t) ~(child : expr) ~(parent : expr option) : t =
    let pid = Option.map (fun p -> p.id) parent in
    { idx with parent_of = NodeIdMap.add child.id pid idx.parent_of }

  let expr (idx : t) (id : node_id) : expr =
    match NodeIdMap.find_opt id idx.node_of with
    | Some e -> e
    | None -> raise @@ Bug ("Missing node id: " ^ string_of_node_id id)

  let range (idx : t) (id : node_id) : range = (expr idx id).range

  let parent (idx : t) (id : node_id) : node_id option =
    match NodeIdMap.find_opt id idx.parent_of with
    | Some e -> e
    | None -> raise @@ Bug ("Missing node id: " ^ string_of_node_id id)

  let rec index_expr ?(parent = None) (idx : t) (e : expr) : t =
    let idx =
      idx |> add_parent ~child:e ~parent |> fun idx ->
      { idx with node_of = NodeIdMap.add e.id e idx.node_of }
    in
    let go child idx = index_expr ~parent:(Some e) idx child in
    match e.node with
    | EIf (c, tbr, fbr) -> idx |> go c |> go tbr |> go fbr
    | ELam (_, _, _, body) -> go body idx
    | EApp (f, a) -> idx |> go f |> go a
    | ELet (_, _, e1, e2) -> idx |> go e1 |> go e2
    | ELetStack { e1; e2; _ } -> idx |> go e1 |> go e2
    | EExport e1 -> go e1 idx
    | EVar _ | EInt _ | EBool _ -> idx

  let build (root : expr) : t = index_expr empty_index root
end
