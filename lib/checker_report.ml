module type S = Checker_report_intf.S

module Make
    (Ast : Ast.S)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty)
    (Checker :
      Checker.S
        with type expr = Ast.expr
         and type ty = Ast.ty
         and type node_id = Ast.node_id) =
struct
  type error = Checker.error
  type diag_core = Diag_core.t

  let of_error (err : Checker.error) : Diag_core.t =
    match err with
    | EUnboundVar { id; x } ->
        Diag_core.error ~code:"E_UNBOUND_VAR" ~primary:id
          (Diag_core.Unbound_var { x })
    | EExpectedBool { id } ->
        Diag_core.error ~code:"E_EXPECTED_BOOL" ~primary:id
          Diag_core.Expected_bool
    | ETypeMismatch { id; expected; got } ->
        Diag_core.error ~code:"E_TYPE_MISMATCH" ~primary:id
          (Diag_core.Type_mismatch { expected; got })
    | EExpectedFun { id } ->
        Diag_core.error ~code:"E_EXPECTED_FUN" ~primary:id
          Diag_core.Expected_fun
    | EStackEscape ev ->
        let related =
          ev.binders
          |> List.map (fun (x, binder_id) ->
              Diag_core.Stack_binder { x; binder_id })
        in
        Diag_core.error ~code:"E_STACK_ESCAPE" ~primary:ev.export_id ~related
          (Diag_core.Stack_escape { escaping_vars = ev.escaping_vars })
end
