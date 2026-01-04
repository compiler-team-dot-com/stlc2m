module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Action_id : sig
      type t = int
    end)
    (Checker :
      Checker.S with type expr = Ast.expr and type node_id = Ast.node_id)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) =
struct
  module Core = Actions_core.Make (Ast) (Checker) (Diag_core)
  module Render = Actions_render.Make (Ast) (Ast_index) (Action_id)

  type kind = Render.kind = Quickfix | Explain
  type t = Render.t

  let propose_actions ~next_id ~root ~index ~(diag : Diag_core.t)
      (err : Checker.error) : t list =
    Core.propose diag err
    |> List.map (fun (p : Core.proposal) ->
        Render.render ~next_id ~root ~index
          ( (match p.kind with
            | Core.Quickfix -> Quickfix
            | Core.Explain -> Explain),
            p.title,
            p.targets,
            p.rationale,
            p.apply ))
end
