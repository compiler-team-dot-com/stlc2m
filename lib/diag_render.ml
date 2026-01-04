module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Diag : Diag.S with module Range = Ast.Range)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) =
struct
  let render_related (idx : Ast_index.t) (r : Diag_core.related) : Diag.related
      =
    match r with
    | Stack_binder { x; binder_id } ->
        {
          Diag.range = Ast_index.range idx binder_id;
          message = "Stack binding of " ^ x;
        }

  let render_message (p : Diag_core.payload) : string =
    match p with
    | Unbound_var { x } -> "Unbound variable: " ^ x
    | Expected_bool -> "Expected Bool in if-condition"
    | Type_mismatch { expected; got } ->
        "If branches have different types: " ^ Ast.string_of_ty expected
        ^ " vs " ^ Ast.string_of_ty got
    | Expected_fun -> "Expected a function in application"
    | Stack_escape { escaping_vars = _ } ->
        "export requires a stack-closed expressions, but stack-bound variables \
         are referenced"

  let render (idx : Ast_index.t) (d : Diag_core.t) : Diag.t =
    let range = Ast_index.range idx d.primary in
    let related = List.map (render_related idx) d.related in
    let msg = render_message d.payload in
    Diag.error ~code:d.code ~range ~related msg
end
