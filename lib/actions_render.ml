module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Action_id : sig
      type t = int
    end) =
struct
  type kind = Action_kind.t

  type t = {
    id : Action_id.t;
    kind : kind;
    title : string;
    targets : Ast.node_id list;
    highlights : Ast.range list;
    rationale : string;
  }

  let render ~next_id ~index
      ((kind, title, targets, rationale) :
        kind * string * Ast.node_id list * string) : t =
    let id = next_id () in
    let highlights = targets |> List.map (Ast_index.range index) in
    { id; kind; title; targets; highlights; rationale }
end
