%{
open Ast
%}

/* Tokens */
%token EOF
%token LET LETSTACK IN
%token IF THEN ELSE
%token FUN EXPORT
%token TRUE FALSE
%token LPAREN RPAREN
%token COLON
%token ARROW
%token EQ
%token <int> INT
%token <string> IDENT

/* Entry */
%start <Ast.expr option> prog

%%

prog:
  | EOF { None }
  | e = expr; EOF { Some e }

/* --------------- Expressions (precedence) --------------- */

/* Lowest precedence: let/letstack (right-assoc in body) */
expr:
  | LET; id = IDENT; EQ; e = expr; IN; body = expr
      { mk_range_node $startpos $endpos (ELet (id, e, body)) }
  | LETSTACK; id = IDENT; EQ; e = expr; IN; body = expr
      { mk_range_node $startpos $endpos (ELetStack (id, e, body)) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
      { mk_range_node $startpos $endpos (EIf (e1, e2, e3)) }
  | FUN; LPAREN; id = IDENT; COLON; ty = ty; RPAREN; ARROW; e = expr
      { mk_range_node $startpos $endpos (ELam (id, ty, e)) }
  | EXPORT; e = expr_app
      { mk_range_node $startpos $endpos (EExport e) }
  | e = expr_app
      { e }

/* Application: left-associative sequence of atoms */
expr_app:
  | e = expr_app; a = atom
      { mk_range_node $startpos $endpos (EApp (e, a)) }
  | a = atom
      { a }

/* Atoms */
atom:
  | id = IDENT
      { mk_range_node $startpos $endpos (EVar id) }
  | i = INT
      { mk_range_node $startpos $endpos (EInt i) }
  | TRUE
      { mk_range_node $startpos $endpos (EBool true) }
  | FALSE
      { mk_range_node $startpos $endpos (EBool false) }
  | LPAREN; e = expr; RPAREN
      { e }

/* -------------- Types ---------------- */

