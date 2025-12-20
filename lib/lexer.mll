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

let whitespace = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let int_lit = digit+
let ident_start = ['A'-'Z' 'a'-'z' '_']
let ident_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let ident = ident_start ident_char+

rule token = parse
  | whitespace { token lexbuf }

  (* Comments: simple ML-style *)
  | "(*" { comment lexbuf; token lexbuf }

  | "->" { ARROW }
  | "=" { EQ }
  | ":" { COLON }
  | "(" { LPAREN }
  | ")" { RPAREN }

  | int_lit as s {
      (* Beware of overflow *)
      INT (int_of_string s)
    }

  | ident as s {
      match Hashtbl.find_opt keyword_table s with
      | Some kw -> kw
      | None -> IDENT s
    }

  | eof { EOF }

  | _ {
      let ch = Lexing.lexeme lexbuf in
      error lexbuf ("Unexpected character: " ^ ch)
    }

and comment = parse
  | "*)" { () }
  | eof { error lexbuf "Unterminated commend" }
  | _ { comment lexbuf }