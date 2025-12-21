open Ast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type stack_binding = { ty : ty; binder_range : range }
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

(* The "export" check *)
let rec infer (env : env) (e : expr) : (infer_result, Diag.t) result =
  match e.node with
  | EInt _ -> Ok (TInt, StringSet.empty)
  | EBool _ -> Ok (TBool, StringSet.empty)
  | EVar x -> (
      match lookup env x with
      | None ->
          Error
            (Diag.error ~code:"E_UNBOUND_VAR" ~range:e.range
               ("Unbound variable: " ^ x))
      | Some (`Heap t) -> Ok (t, StringSet.empty)
      | Some (`Stack b) -> Ok (b.ty, StringSet.singleton x))
  | EIf (c, tbr, fbr) ->
      let* ct, cdeps = infer env c in
      if ct <> TBool then
        Error
          (Diag.error ~code:"E_EXPECTED_BOOL" ~range:c.range
             "Expected Bool in if-condition")
      else
        let* tt, tdeps = infer env tbr in
        let* ft, fdeps = infer env fbr in
        if tt <> ft then
          Error
            (Diag.error ~code:"E_TYPE_MISMATCH" ~range:e.range
               ("If branches have different types: " ^ Ast.string_of_ty tt
              ^ " vs " ^ Ast.string_of_ty ft))
        else Ok (tt, union_deps cdeps (union_deps tdeps fdeps))
  | ELam (x, xty, body) ->
      let env' = extend_heap env x xty in
      let* bty, bdeps = infer env' body in
      Ok (TArrow (xty, bty), bdeps)
  | EApp (f, a) -> (
      let* fty, fdeps = infer env f in
      let* aty, adeps = infer env a in
      match fty with
      | TArrow (dom, cod) ->
          if dom <> aty then
            Error
              (Diag.error ~code:"E_TYPE_MISMATCH" ~range:e.range
                 ("Function expects " ^ Ast.string_of_ty dom
                ^ " but argument has " ^ Ast.string_of_ty aty))
          else Ok (cod, union_deps fdeps adeps)
      | _ ->
          Error
            (Diag.error ~code:"E_EXPECTED_FUN" ~range:f.range
               "Expected a function in application"))
  | ELet (x, e1, e2) ->
      let* t1, d1 = infer env e1 in
      let env' = extend_heap env x t1 in
      let* t2, d2 = infer env' e2 in
      Ok (t2, union_deps d1 d2)
  | ELetStack (x, e1, e2) ->
      let* t1, d1 = infer env e1 in
      let env' =
        extend_stack env x { ty = t1; binder_range = range_of_ident e.range x }
      in
      let* t2, d2 = infer env' e2 in
      Ok (t2, union_deps d1 d2)
  | EExport e1 ->
      let* t1, deps = infer env e1 in
      if StringSet.is_empty deps then Ok (t1, StringSet.empty)
      else
        (* Pick witnesses and point to their binders *)
        let related =
          deps |> StringSet.to_list
          |> List.filter_map (fun v ->
              match StringMap.find_opt v env.stack with
              | None -> None
              | Some b ->
                  Some
                    {
                      Diag.range = b.binder_range;
                      message = "Stack binding of " ^ v;
                    })
        in
        Error
          (Diag.error ~code:"E_STACK_ESCAPE" ~range:e.range ~related
             "export requires a stack-closed expressions, but stack-bound \
              variables are referenced")

and ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
and range_of_ident (r : range) (_x : string) : range = r
