module Ast = Compile.Ast
module Diag = Compile.Diag
module J = Yojson.Safe
module JU = Yojson.Safe.Util

let pos_to_json (p : Lexing.position) : J.t =
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  `Assoc [ ("line", `Int line); ("col", `Int col) ]

let range_to_json (r : Diag.range) : J.t =
  `Assoc [ ("start", pos_to_json r.start_pos); ("end", pos_to_json r.end_pos) ]

let related_to_json (r : Diag.related) : J.t =
  `Assoc [ ("range", range_to_json r.range); ("message", `String r.message) ]

let diag_to_json (d : Diag.t) : J.t =
  `Assoc
    [
      ("code", `String d.code);
      ("message", `String d.message);
      ("severity", `String (Diag.severity_to_string d.severity));
      ("range", range_to_json d.range);
      ("related", `List (List.map related_to_json d.related));
    ]

let mk_error ~id ~code ~message : J.t =
  `Assoc
    [
      ("id", id);
      ("ok", `Bool false);
      ("error", `Assoc [ ("code", `String code); ("message", `String message) ]);
    ]

let mk_ok ~id ~diags : J.t =
  `Assoc
    [
      ("id", id);
      ("ok", `Bool true);
      ("diagnostics", `List (List.map diag_to_json diags));
    ]

(* Parse+check from raw source text. *)
let check_text (text : string) : Diag.t list =
  match Compile.from_string ~version:0 ~fname:"<buffer>" text with
  | Error _parse_err -> []
  | Ok { snapshot = _; result = None } -> []
  | Ok { snapshot = _; result = Some (Ok _ok) } -> []
  | Ok { snapshot = None; result = Some (Error _err) } -> []
  | Ok { snapshot = Some snap; result = Some (Error err) } ->
      [ Compile.diag_of_error snap err ]

let handle_request (json : J.t) : J.t =
  let id = match JU.member "id" json with `Null -> `Null | v -> v in
  try
    let method_ = JU.member "method" json |> JU.to_string in
    match method_ with
    | "check" ->
        let text = JU.member "text" json |> JU.to_string in
        let diags = check_text text in
        mk_ok ~id ~diags
    | _ ->
        mk_error ~id ~code:"E_PROTOCOL" ~message:("Unknown method: " ^ method_)
  with
  | Yojson.Json_error msg ->
      mk_error ~id ~code:"E_PROTOCOL" ~message:("JSON error: " ^ msg)
  | JU.Type_error (msg, _json) ->
      mk_error ~id ~code:"E_PROTOCOL" ~message:("Bad request: " ^ msg)
  | Lexer.Lexing_error (msg, pos) ->
      let message =
        Printf.sprintf "Lex error at %s: %s"
          (Ast.Range.string_of_position pos)
          msg
      in
      mk_error ~id ~code:"E_LEX" ~message
  | Parsing.Parse_error -> mk_error ~id ~code:"E_PARSE" ~message:"Parse error"
  | exn -> mk_error ~id ~code:"E_INTERNAL" ~message:(Printexc.to_string exn)

let run () : unit =
  (* Make stdout line-buffered; flushing per response is important. *)
  try
    while true do
      let line = input_line stdin in
      if String.trim line = "" then () (* Empty line *)
      else
        let json = J.from_string line in
        let resp = handle_request json in
        output_string stdout (J.to_string resp);
        output_char stdout '\n';
        flush stdout
    done
  with End_of_file -> ()
