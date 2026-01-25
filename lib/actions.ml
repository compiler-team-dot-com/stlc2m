module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Action_id : sig
      type t
    end)
    (Checker :
      Checker.S with type expr = Ast.expr and type node_id = Ast.node_id)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) =
struct
  module Core = Actions_core.Make (Ast) (Checker) (Diag_core)
  module Render = Actions_render.Make (Ast) (Ast_index) (Action_id)

  type kind = Action_kind.t
  type t = Render.t
  type apply_fn = Ast.expr -> Ast.expr option

  let propose_global_actions ~next_id ~index ~(root : Ast.expr) :
      (t * apply_fn) list =
    Core.propose_global ~root
    |> List.map (fun (p : Core.proposal) ->
        let rendered =
          Render.render ~next_id ~index (p.kind, p.title, p.targets, p.rationale)
        in
        (rendered, p.apply))

  let propose_actions ~next_id ~index ~(diag : Diag_core.t)
      (err : Checker.error) : (t * apply_fn) list =
    Core.propose diag err
    |> List.map (fun (p : Core.proposal) ->
        let rendered =
          Render.render ~next_id ~index (p.kind, p.title, p.targets, p.rationale)
        in
        (rendered, p.apply))
end
