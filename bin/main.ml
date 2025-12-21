open Stlc2m

let parse_from_channel (ic : in_channel) : Ast.expr option =
  let lexbuf = Lexing.from_channel ic in
  try Parser.prog Lexer.token lexbuf with
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

let () =
  let file =
    match Array.to_list Sys.argv with
    | [ _ ] -> None
    | [ _; f ] -> Some f
    | _ ->
        Printf.eprintf "Usage: stlc2m [file]\n";
        exit 2
  in
  let ic, _file_label =
    match file with None -> (stdin, "<stdin>") | Some f -> (open_in f, f)
  in
  match parse_from_channel ic with
  | None -> exit 0
  | Some e -> (
      match Checker.infer Checker.empty_env e with
      | Ok (_ty, _deps) -> exit 0
      | Error d ->
          print_endline (Diag.to_human d);
          exit 1)
