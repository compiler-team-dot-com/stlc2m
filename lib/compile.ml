module IdGen = Id_gen.Make ()
module Ast = Ast.Make (IdGen.Node_id)
module Ast_index = Ast_index.Make (Ast)
module Diag = Diag.Make (Ast)
module Diag_core = Diag_core.Make (Ast)
module Diag_render = Diag_render.Make (Ast) (Ast_index) (Diag) (Diag_core)
module Checker = Checker.Make (Ast)
module Checker_report = Checker_report.Make (Ast) (Diag_core) (Checker)
module Env = Parse_env.Make (Ast) (IdGen)

module Actions =
  Actions.Make (Ast) (Ast_index) (Action_id) (Checker) (Diag_core)

type error = Checker.error
type infer_result = Checker.infer_result

module StringSet = Set.Make (String)

type parse_error =
  | LexError of { msg : string; pos : Lexing.position }
  | ParseError of { pos : Lexing.position }

type snapshot = {
  (* source : string; *)
  _root : Ast.expr;
  index : Ast_index.t; (* version : int; *)
}

type t = {
  snapshot : snapshot option;
  result : (Checker.infer_result, error) result option;
}

let mk_lexbuf_from_string ?fname (s : string) =
  let lexbuf = Lexing.from_string s in
  (match fname with
  | None -> ()
  | Some fn -> lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fn });
  lexbuf

let parse_prog (lexbuf : Lexing.lexbuf) : (Ast.expr option, parse_error) result
    =
  let module P = Parser.Make (Env) in
  try Ok (P.prog Lexer.token lexbuf) with
  | Lexer.Lexing_error (msg, pos) -> Error (LexError { msg; pos })
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Error (ParseError { pos })

let build_snapshot ~(source : string) ~(root : Ast.expr) ~(version : int) :
    snapshot =
  let _ = source in
  let _ = version in
  let index = Ast_index.build root in
  (* { source; root; index; version } *)
  { _root = root; index }

let from_string ?(version = 0) ?fname (source : string) :
    (t, parse_error) result =
  let lexbuf = mk_lexbuf_from_string ?fname source in
  match parse_prog lexbuf with
  | Error _ as e -> e
  | Ok None -> Ok { snapshot = None; result = None }
  | Ok (Some root) ->
      let snapshot = build_snapshot ~source ~root ~version in
      let result = Checker.infer root in
      Ok { snapshot = Some snapshot; result = Some result }

let from_channel ?(version = 0) ?fname (ic : in_channel) :
    (t, parse_error) result =
  let source = In_channel.input_all ic in
  from_string ~version ?fname source

let diag_of_error (snap : snapshot) (err : error) : Diag.t =
  err |> Checker_report.of_error |> Diag_render.render snap.index

type action_kind = Action_kind.t

type action = {
  id : int;
  kind : action_kind;
  title : string;
  targets : Ast.node_id list;
  highlights : Ast.range list;
  rationale : string;
}

let report_of_error (snap : snapshot) (err : error) : Diag.t * action list =
  let diag_core = Checker_report.of_error err in
  let diag = Diag_render.render snap.index diag_core in

  (* Fresh action ids for this response only. Later you can replace with a stateful
     registry (server-managed) keyed by a stable Action_id.t. *)
  let next =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in

  let actions =
    Actions.propose_actions ~next_id:next ~index:snap.index ~diag:diag_core err
    |> List.map (fun (a : Actions.t) ->
        {
          id = Action_id.to_int a.id;
          kind = a.kind;
          title = a.title;
          targets = a.targets;
          highlights = a.highlights;
          rationale = a.rationale;
        })
  in
  (diag, actions)
