(* Diagnostic record + rendering *)

open Ast

type severity = Error | Warning | Info
type related = { range : range; message : string }

type t = {
  code : string;
  message : string;
  severity : severity;
  range : range;
  related : related list;
}

let error ?(related = []) ~code ~range message : t =
  { code; message; severity = Error; range; related }

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"

let pp_position (p : Lexing.position) : int * int =
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  (line, col)

let pp_range (r : range) : int * int * int * int =
  let l1, c1 = pp_position r.start_pos in
  let l2, c2 = pp_position r.end_pos in
  (l1, c1, l2, c2)

let to_human ?(file = "<input>") (d : t) : string =
  let l1, c1, _, _ = pp_range d.range in
  let header = Printf.sprintf "%s:%d:%d: [%s] %s" file l1 c1 d.code d.message in
  let rel =
    d.related
    |> List.map (fun (r : related) ->
        let rl1, rc1, _, _ = pp_range r.range in
        Printf.sprintf " related: %s:%d:%d: %s" file rl1 rc1 r.message)
    |> String.concat "\n"
  in
  if rel == "" then header else header ^ "\n" ^ rel
