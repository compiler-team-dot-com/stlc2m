module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Action_id : sig
      type t = int
    end) : sig
  type kind = Quickfix | Explain

  type t = {
    id : Action_id.t;
    kind : kind;
    title : string;
    targets : Ast.node_id list; (* semantic targets *)
    highlights : Ast.range list; (* derived UI highlighting *)
    rationale : string;
    apply : unit -> Ast.expr option; (* None for Explain *)
  }

  val render :
    next_id:(unit -> Action_id.t) ->
    root:Ast.expr ->
    index:Ast_index.t ->
    kind * string * Ast.node_id list * string * (Ast.expr -> Ast.expr option) ->
    t
end
