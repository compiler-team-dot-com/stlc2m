type range = { start_pos : Lexing.position; end_pos : Lexing.position }
type ty = TInt | TBool | TArrow of ty * ty

type expr = { node : expr_node; range : range }

and expr_node =
  | EVar of string
  | EInt of int
  | EBool of bool
  | EIf of expr * expr * expr
  | ELam of string * ty * expr (* fun (x : ty) -> body *)
  | EApp of expr * expr
  | ELet of string * expr * expr (* let x = e1 in e2 *)
  | ELetStack of string * expr * expr (* letstack x = e1 in e2 *)
  | EExport of expr (* export e *)
