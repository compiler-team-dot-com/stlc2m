open Stlc2m

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    match Parser.prog Lexer.token lexbuf with
    | None -> print_endline "OK (empty)"
    | Some _ -> print_endline "OK (parsed)"
  with
  | Lexer.Lexing_error (msg, pos) ->
      Printf.eprintf "Lex error at %d:%d: %s\n" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        msg;
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Parse error at %d:%d\n" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
