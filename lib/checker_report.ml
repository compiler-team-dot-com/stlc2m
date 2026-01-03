module type S = Checker_report_intf.S

module Make
    (Ast : Ast.S)
    (Ast_index :
      Ast_index.S with type range = Ast.Range.t and type node_id = Ast.node_id)
    (Diag : Diag.S with module Range = Ast.Range)
    (Checker :
      Checker.S
        with type expr = Ast.expr
         and type ty = Ast.ty
         and type range = Ast.Range.t
         and type node_id = Ast.node_id) =
struct
  type ast_index = Ast_index.t
  type error = Checker.error
  type diag = Diag.t

  let of_error (idx : Ast_index.t) (err : Checker.error) : Diag.t =
    match err with
    | EUnboundVar { range; x } ->
        Diag.error ~code:"E_UNBOUND_VAR" ~range ("Unbound variable: " ^ x)
    | EExpectedBool { range } ->
        Diag.error ~code:"E_EXPECTED_BOOL" ~range
          "Expected Bool in if-condition"
    | ETypeMismatch { range; expected; got } ->
        Diag.error ~code:"E_TYPE_MISMATCH" ~range
          ("If branches have different types: " ^ Ast.string_of_ty expected
         ^ " vs " ^ Ast.string_of_ty got)
    | EExpectedFun { range } ->
        Diag.error ~code:"E_EXPECTED_FUN" ~range
          "Expected a function in application"
    | EStackEscape ev ->
        let related =
          ev.binders
          |> List.map (fun (x, id) ->
              {
                Diag.range = Ast_index.range idx id;
                message = "Stack binding of " ^ x;
              })
        in
        Diag.error ~code:"E_STACK_ESCAPE"
          ~range:(Ast_index.range idx ev.export_id)
          ~related
          "export requires a stack-closed expressions, but stack-bound \
           variables are referenced"
end
