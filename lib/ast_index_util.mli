module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id) : sig
  val climb :
    Ast_index.t ->
    from:Ast.node_id ->
    (Ast.node_id -> bool) ->
    Ast.node_id option
end
