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

let mk_range (start_pos : Lexing.position) (end_pos : Lexing.position) : range =
  { start_pos; end_pos }

let mk_range_node (sp : Lexing.position) (ep : Lexing.position)
    (node : expr_node) : expr =
  { node; range = mk_range sp ep }

let string_of_position (p : Lexing.position) : string =
  (* Lexing.position.pos_lnum is 1-based; pos_cnum and pos_bol allow column computation *)
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  Printf.sprintf "%d:%d" line col

let string_of_range (r : range) : string =
  Printf.sprintf "%s-%s"
    (string_of_position r.start_pos)
    (string_of_position r.end_pos)

let rec string_of_ty = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (t1, t2) ->
      let left =
        match t1 with
        | TArrow _ -> "(" ^ string_of_ty t1 ^ ")"
        | _ -> string_of_ty t1
      in
      left ^ " -> " ^ string_of_ty t2
