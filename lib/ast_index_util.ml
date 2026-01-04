module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id) =
struct
  let rec climb (idx : Ast_index.t) ~(from : Ast.node_id)
      (p : Ast.node_id -> bool) : Ast.node_id option =
    if p from then Some from
    else
      match Ast_index.parent idx from with
      | None -> None
      | Some pid -> climb idx ~from:pid p
end
