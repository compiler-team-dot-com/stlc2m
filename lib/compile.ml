open Ast
open Checker

type parse_error =
  | LexError of { msg : string; pos : Lexing.position }
  | ParseError of { pos : Lexing.position }

type snapshot = {
  (* source : string; *)
  (* root : expr; *)
  index : Ast_index.t; (* version : int; *)
}

type t = {
  snapshot : snapshot option;
  result : (ty * StringSet.t, error) result option;
}

let mk_lexbuf_from_string ?fname (s : string) =
  let lexbuf = Lexing.from_string s in
  (match fname with
  | None -> ()
  | Some fn -> lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fn });
  lexbuf

let parse_prog (lexbuf : Lexing.lexbuf) : (expr option, parse_error) result =
  let module P = Parser.Make () in
  try Ok (P.prog Lexer.token lexbuf) with
  | Lexer.Lexing_error (msg, pos) -> Error (LexError { msg; pos })
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Error (ParseError { pos })

let build_snapshot ~(source : string) ~(root : expr) ~(version : int) : snapshot
    =
  let _ = source in
  let _ = version in
  let index = Ast_index.build root in
  (* { source; root; index; version } *)
  { index }

let from_string ?(version = 0) ?fname (source : string) :
    (t, parse_error) result =
  let lexbuf = mk_lexbuf_from_string ?fname source in
  match parse_prog lexbuf with
  | Error _ as e -> e
  | Ok None -> Ok { snapshot = None; result = None }
  | Ok (Some root) ->
      let snapshot = build_snapshot ~source ~root ~version in
      let result = Checker.infer Checker.empty_env root in
      Ok { snapshot = Some snapshot; result = Some result }

let from_channel ?(version = 0) ?fname (ic : in_channel) :
    (t, parse_error) result =
  let source = In_channel.input_all ic in
  from_string ~version ?fname source

let diag_of_error (snap : snapshot) (err : error) : Diag.t =
  Checker.diag_of_error snap.index err
