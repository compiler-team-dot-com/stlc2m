module Make
    (Ast : Ast.S)
    (Checker :
      Checker.S with type expr = Ast.expr and type node_id = Ast.node_id)
    (Diag_core :
      Diag_core.S with type node_id = Ast.node_id and type ty = Ast.ty) =
struct
  type kind = Action_kind.t

  (* A node-id "set" implemented via Ast.IdMap. *)
  module NodeIdSet = struct
    type t = unit Ast.IdMap.t

    let empty = Ast.IdMap.empty
    let add (id : Ast.node_id) (s : t) : t = Ast.IdMap.add id () s
    let mem (id : Ast.node_id) (s : t) : bool = Ast.IdMap.mem id s
  end

  type proposal = {
    kind : kind;
    title : string;
    targets : Ast.node_id list;
    rationale : string;
    apply : Ast.expr -> Ast.expr option;
  }

  let rec rewrite_letstack_to_let ~(targets : NodeIdSet.t) (e : Ast.expr) :
      Ast.expr =
    let rw = rewrite_letstack_to_let ~targets in
    let node =
      let open Ast in
      match e.node with
      | ELetStack ({ e1; e2; _ } as ls) when NodeIdSet.mem e.id targets ->
          (* Preserve kw_range/x/x_range, but change constructor to heap-binding let *)
          ELet (ls.x, ls.x_range, rw e1, rw e2)
      | ELetStack ({ e1; e2; _ } as ls) ->
          ELetStack { ls with e1 = rw e1; e2 = rw e2 }
      | EIf (c, tbr, fbr) -> EIf (rw c, rw tbr, rw fbr)
      | ELam (x, xr, xty, body) -> ELam (x, xr, xty, rw body)
      | EApp (f, a) -> EApp (rw f, rw a)
      | ELet (x, xr, e1, e2) -> ELet (x, xr, rw e1, rw e2)
      | EExport e1 -> EExport (rw e1)
      | (EVar _ | EInt _ | EBool _) as n -> n
    in
    { e with node }

  let propose (_d : Diag_core.t) (err : Checker.error) : proposal list =
    match err with
    | Checker.EStackEscape ev ->
        let binder_ids = ev.binders |> List.map snd in
        let targets = ev.export_id :: binder_ids in
        let vars = ev.escaping_vars |> List.sort String.compare in
        let vars_s = String.concat ", " vars in

        let explain =
          {
            kind = Explain;
            title = "Explain why export is not stack-closed";
            targets;
            rationale =
              "The exported expression references stack-bound variables: "
              ^ vars_s
              ^ ". A value crossing an export boundary must not depend on \
                 stack bindings.";
            apply = (fun _root -> None);
          }
        in

        let quickfix_letstack_to_let =
          let targets_set =
            binder_ids
            |> List.fold_left
                 (fun acc id -> NodeIdSet.add id acc)
                 NodeIdSet.empty
          in
          {
            kind = Quickfix;
            title = "Convert letstack to let for: " ^ vars_s;
            targets;
            rationale =
              "Replacing stack bindings with heap bindings prevents stack \
               values from escaping via export.";
            apply =
              (fun root ->
                Some (rewrite_letstack_to_let ~targets:targets_set root));
          }
        in
        [ quickfix_letstack_to_let; explain ]
    | _ -> []

  let propose_global ~(root : Ast.expr) : proposal list =
    [
      {
        kind = Format;
        title = "Format document";
        targets = [ root.id ];
        rationale =
          "Normalize whitespace and layout using the compiler formatter.";
        apply = (fun root -> Some root);
      };
    ]
end
