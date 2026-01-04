module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Action_id : sig
      type t = int
    end) =
struct
  type kind = Quickfix | Explain

  type t = {
    id : Action_id.t;
    kind : kind;
    title : string;
    targets : Ast.node_id list;
    highlights : Ast.range list;
    rationale : string;
    apply : unit -> Ast.expr option;
  }

  let render ~next_id ~root ~index
      ((kind, title, targets, rationale, apply_fn) :
        kind
        * string
        * Ast.node_id list
        * string
        * (Ast.expr -> Ast.expr option)) : t =
    let id = next_id () in
    let highlights = targets |> List.map (Ast_index.range index) in
    let apply =
      match kind with
      | Explain -> fun () -> None
      | Quickfix -> fun () -> apply_fn root
    in
    { id; kind; title; targets; highlights; rationale; apply }
end
