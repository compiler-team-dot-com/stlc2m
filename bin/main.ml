open Stlc2m
module Diag = Compile.Diag

let usage () =
  Printf.eprintf "Usage: stlc2m [--server] [file]\n";
  exit 2

type mode = Server | Stdin | File of string

let run_ic ic =
  match Compile.from_channel ~version:0 ic with
  | Error (Compile.LexError { msg; pos }) ->
      Printf.eprintf "Lex error at %d:%d: %s\n" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        msg;
      exit 1
  | Error (Compile.ParseError { pos }) ->
      Printf.eprintf "Parse error at %d:%d\n" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  | Ok { snapshot = Some snap; result = Some (Error err) } ->
      print_endline (Diag.to_human @@ Compile.diag_of_error snap err);
      exit 1
  | _ -> exit 0

let () =
  let m =
    match Array.to_list Sys.argv with
    | [ _ ] -> Stdin
    | [ _; "--server" ] -> Server
    | [ _; f ] -> File f
    | _ -> usage ()
  in
  match m with
  | Server -> Server.run ()
  | Stdin -> run_ic stdin
  | File f -> run_ic (open_in f)
