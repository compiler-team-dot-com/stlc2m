module Make (Ast : Ast.S) = struct
  open Ast

  let pp_ty = Ast.string_of_ty

  type prec = P_let | P_app | P_atom

  let parens_if (b : bool) (s : string) = if b then "(" ^ s ^ ")" else s

  let rec pp_expr_prec (ctx : prec) (e : expr) : string =
    match e.node with
    | EInt n -> string_of_int n
    | EBool true -> "true"
    | EBool false -> "false"
    | EVar x -> x
    | EIf (c, tbr, fbr) ->
        let s =
          "if " ^ pp_expr_prec P_let c ^ " then " ^ pp_expr_prec P_let tbr
          ^ " else " ^ pp_expr_prec P_let fbr
        in
        parens_if (ctx <> P_let) s
    | ELam (x, _xr, xty, body) ->
        let s =
          "fun (" ^ x ^ " : " ^ pp_ty xty ^ ") -> " ^ pp_expr_prec P_let body
        in
        parens_if (ctx <> P_let) s
    | ELet (x, _xr, e1, e2) ->
        let s =
          "let " ^ x ^ " = " ^ pp_expr_prec P_let e1 ^ " in "
          ^ pp_expr_prec P_let e2
        in
        parens_if (ctx <> P_let) s
    | ELetStack { x; e1; e2; _ } ->
        let s =
          "letstack " ^ x ^ " = " ^ pp_expr_prec P_let e1 ^ " in "
          ^ pp_expr_prec P_let e2
        in
        parens_if (ctx <> P_let) s
    | EExport e1 ->
        (* Always parenthesize to avoid grammar corner-cases. *)
        "export (" ^ pp_expr_prec P_let e1 ^ ")"
    | EApp (f, a) ->
        let sf = pp_expr_prec P_app f in
        let sa = pp_expr_prec P_atom a in
        let s = sf ^ " " ^ sa in
        parens_if (ctx = P_atom) s

  let pp_expr (e : expr) : string = pp_expr_prec P_let e
end
