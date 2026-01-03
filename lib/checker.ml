module type S = Checker_intf.S

module Make (Ast : Ast.S) = struct
  open Ast

  type expr = Ast.expr
  type ty = Ast.ty
  type range = Ast.Range.t
  type node_id = Ast.node_id

  module StringSet = Set.Make (String)
  module StringMap = Map.Make (String)

  type stack_binding = {
    ty : ty;
    binder_id : node_id;
    _binder_range : range;
    _kw_range : range;
  }

  type env = { heap : ty StringMap.t; stack : stack_binding StringMap.t }

  let empty_env : env = { heap = StringMap.empty; stack = StringMap.empty }

  let lookup (env : env) (x : string) :
      [ `Heap of ty | `Stack of stack_binding ] option =
    match StringMap.find_opt x env.stack with
    | Some b -> Some (`Stack b)
    | None -> (
        match StringMap.find_opt x env.heap with
        | Some t -> Some (`Heap t)
        | None -> None)

  let extend_heap (env : env) (x : string) (t : ty) : env =
    { env with heap = StringMap.add x t env.heap }

  let extend_stack (env : env) (x : string) (b : stack_binding) : env =
    { env with stack = StringMap.add x b env.stack }

  type infer_result = ty * StringSet.t

  let union_deps (a : StringSet.t) (b : StringSet.t) = StringSet.union a b

  module Evidence = struct
    type stack_escape = {
      export_id : node_id;
      _escaping_vars : string list; (* canonical list, stable order *)
      binders : (string * node_id) list; (* stack var -> binder node id *)
    }
  end

  type error =
    | EUnboundVar of { range : range; x : string }
    | EExpectedBool of { range : range }
    | ETypeMismatch of { range : range; expected : ty; got : ty }
    | EExpectedFun of { range : range }
    | EStackEscape of Evidence.stack_escape

  (* The "export" check *)
  let rec inf (env : env) (e : expr) : (infer_result, error) result =
    match e.node with
    | EInt _ -> Ok (TInt, StringSet.empty)
    | EBool _ -> Ok (TBool, StringSet.empty)
    | EVar x -> (
        match lookup env x with
        | None -> Error (EUnboundVar { range = e.range; x })
        | Some (`Heap t) -> Ok (t, StringSet.empty)
        | Some (`Stack b) -> Ok (b.ty, StringSet.singleton x))
    | EIf (c, tbr, fbr) ->
        let* ct, cdeps = inf env c in
        if ct <> TBool then Error (EExpectedBool { range = c.range })
        else
          let* tt, tdeps = inf env tbr in
          let* ft, fdeps = inf env fbr in
          if tt <> ft then
            Error (ETypeMismatch { range = e.range; expected = tt; got = ft })
          else Ok (tt, union_deps cdeps (union_deps tdeps fdeps))
    | ELam (x, _xr, xty, body) ->
        let env' = extend_heap env x xty in
        let* bty, bdeps = inf env' body in
        Ok (TArrow (xty, bty), bdeps)
    | EApp (f, a) -> (
        let* fty, fdeps = inf env f in
        let* aty, adeps = inf env a in
        match fty with
        | TArrow (dom, cod) ->
            if dom <> aty then
              Error
                (ETypeMismatch { range = e.range; expected = dom; got = aty })
            else Ok (cod, union_deps fdeps adeps)
        | _ -> Error (EExpectedFun { range = f.range }))
    | ELet (x, _xr, e1, e2) ->
        let* t1, d1 = inf env e1 in
        let env' = extend_heap env x t1 in
        let* t2, d2 = inf env' e2 in
        Ok (t2, union_deps d1 d2)
    | ELetStack { kw_range; x; x_range; e1; e2 } ->
        let* t1, d1 = inf env e1 in
        let env' =
          extend_stack env x
            {
              ty = t1;
              binder_id = e.id;
              _binder_range = x_range;
              _kw_range = kw_range;
            }
        in
        let* t2, d2 = inf env' e2 in
        Ok (t2, union_deps d1 d2)
    | EExport e1 ->
        let* t1, deps = inf env e1 in
        if StringSet.is_empty deps then Ok (t1, StringSet.empty)
        else
          (* Pick witnesses and point to their binders *)
          let escaping_vars =
            deps |> StringSet.to_list |> List.sort String.compare
          in
          let binders =
            escaping_vars
            |> List.filter_map (fun v ->
                match StringMap.find_opt v env.stack with
                | None -> None
                | Some b -> Some (v, b.binder_id))
          in
          Error
            (EStackEscape
               { export_id = e1.id; binders; _escaping_vars = escaping_vars })

  and ( let* ) r (f : infer_result -> (infer_result, error) result) =
    match r with Ok v -> f v | Error _ as e -> e

  let infer (e : expr) : (infer_result, error) result = inf empty_env e
end
