%parameter <Env : Parse_env.S>

%start <Env.Ast.expr option> prog

%{
open Env.Ast

let env = Env.create ()
let mk_range_node = Env.mk_range_node
let mk_range = Env.mk_range
%}

%%

prog:
  | EOF
      { None }
  | e = expr; EOF
      { Some e }

/* --------------- Expressions (precedence) --------------- */

/* Lowest precedence: let/letstack (right-assoc in body) */
expr:
  | LET; x = IDENT; EQ; e = expr; IN; body = expr
      {
	let xr = mk_range $startpos(x) $endpos(x) in
	mk_range_node env $startpos $endpos (ELet (x, xr, e, body))
      }
  | LETSTACK; x = IDENT; EQ; e1 = expr; IN; e2 = expr
      {
	let x_range = mk_range $startpos(x) $endpos(x) in
	let kw_range = mk_range $startpos($1) $endpos($1) in
        mk_range_node env $startpos $endpos (ELetStack {kw_range; x; x_range; e1; e2})
      }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
      { mk_range_node env $startpos $endpos (EIf (e1, e2, e3)) }
  | FUN; LPAREN; x = IDENT; COLON; ty = ty; RPAREN; ARROW; e = expr
      {
	let xr = mk_range $startpos(x) $endpos(x) in
        mk_range_node env $startpos $endpos (ELam (x, xr, ty, e))
      }
/* EXPORT expr_app (not EXPORT expr) is deliberate: it avoids parsing "export let ..."
*  without parentheses. To have "export (let ...)", one can always write parentheses.
*  Keeps the grammar unambiguous early.
*/
  | EXPORT; e = expr_app
      { mk_range_node env $startpos $endpos (EExport e) }
  | e = expr_app
      { e }

/* Application: left-associative sequence of atoms */
expr_app:
  | e = expr_app; a = atom
      { mk_range_node env $startpos $endpos (EApp (e, a)) }
  | a = atom
      { a }

/* Atoms */
atom:
  | x = IDENT
      { mk_range_node env $startpos $endpos (EVar x) }
  | i = INT
      { mk_range_node env $startpos $endpos (EInt i) }
  | TRUE
      { mk_range_node env $startpos $endpos (EBool true) }
  | FALSE
      { mk_range_node env $startpos $endpos (EBool false) }
  | LPAREN; e = expr; RPAREN
      { e }

/* -------------- Types ---------------- */

ty:
  | ty = ty_arrow
      { ty }

ty_arrow:
  | t1 = ty_atom; ARROW; t2 = ty_arrow
      { TArrow (t1, t2) } /* right-assoc */
  | ty = ty_atom
      { ty }

ty_atom:
/* IDENT for both term identifiers and type names for simplicity. */
  | t = IDENT
      {
        match t with
	| "Int" -> TInt
	| "Bool" -> TBool
	| _ -> failwith ("Unknown type name: " ^ t)
      }
  | LPAREN; ty = ty; RPAREN
      { ty }
