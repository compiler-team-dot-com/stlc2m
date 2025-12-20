{
open Parser

exception Lexing_error of string * Lexing.position

let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  raise (Lexing_error (msg, pos))

let keyword_table : (string, Parser.token) Hashtbl.t =
  let tbl = Hashtbl.create 32 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v)
    [ ("let", LET)
    ; ("letstack", LETSTACK)
    ; ("in", IN)
    ; ("if", IF)
    ; ("then", THEN)
    ; ("else", ELSE)
    ; ("fun", FUN)
    ; ("export", EXPORT)
    ; ("true", TRUE)
    ; ("false", FALSE)
    ];
  tbl
}

rule read = parse
  | eof { EOF }