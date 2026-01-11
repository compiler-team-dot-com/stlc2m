module Ast = Compile.Ast
module Pretty = Pretty.Make (Ast)
module Diag = Compile.Diag
module Diag_json = Diag_json.Make (Diag)
module Action_registry = Action_registry.Make (Ast)

module Snapshot_registry = Snapshot_registry.Make (struct
  type t = Compile.snapshot
end)

module J = Yojson.Safe
module JU = Yojson.Safe.Util

let registry = Action_registry.create ()
let snapshots = Snapshot_registry.create ()
let next_snapshot_id = ref 0

let get_opt_int (json : J.t) (field : string) : int option =
  match JU.member field json with `Null -> None | v -> Some (JU.to_int v)

let get_opt_string (json : J.t) (field : string) : string option =
  match JU.member field json with `Null -> None | v -> Some (JU.to_string v)

let mk_error ~id ~code ~message : J.t =
  `Assoc
    [
      ("id", id);
      ("ok", `Bool false);
      ("error", `Assoc [ ("code", `String code); ("message", `String message) ]);
    ]

let mk_ok_check ~id ~diags ~actions ~(snapshot_id : Snapshot_id.t option)
    ~(version : int option) ~(content_hash : string option) : J.t =
  let base =
    [
      ("id", id);
      ("ok", `Bool true);
      ("diagnostics", `List (List.map Diag_json.diag_to_json diags));
      ("actions", `List (List.map Actions_json.action_to_json actions));
    ]
  in
  let with_tokens =
    ( base |> fun acc ->
      match snapshot_id with
      | None -> acc
      | Some s -> ("snapshot_id", `Int (Snapshot_id.to_int s)) :: acc )
    |> fun acc ->
    (match version with None -> acc | Some v -> ("version", `Int v) :: acc)
    |> fun acc ->
    match content_hash with
    | None -> acc
    | Some h -> ("content_hash", `String h) :: acc
  in
  `Assoc (List.rev with_tokens)

let mk_ok_replace_all ~id ~text : J.t =
  `Assoc [ ("id", id); ("ok", `Bool true); ("replace_all", `String text) ]

(* Parse+check from raw source text, registering actions in server state. *)
let check_text ~(text : string) ~(version : int option)
    ~(content_hash : string option) :
    Diag.t list * Compile.action list * Snapshot_id.t option =
  Action_registry.clear registry;
  Snapshot_registry.clear snapshots;

  match Compile.from_string ~version:0 ~fname:"<buffer>" text with
  | Error _parse_err -> ([], [], None)
  | Ok { snapshot = _; result = None } -> ([], [], None)
  | Ok { snapshot = _; result = Some (Ok _ok) } -> ([], [], None)
  | Ok { snapshot = None; result = Some (Error _err) } -> ([], [], None)
  | Ok { snapshot = Some snap; result = Some (Error err) } ->
      let diag, actions, impls = Compile.report_of_error snap err in
      incr next_snapshot_id;
      let snapshot_id = !next_snapshot_id in
      Snapshot_registry.add snapshots snapshot_id snap;

      impls
      |> List.iter (fun (id, apply) ->
          Action_registry.add registry id
            { apply; version; content_hash; snapshot_id });

      ([ diag ], actions, Some snapshot_id)

let validate_tokens ~(entry : Action_registry.entry)
    ~(snapshot_id : Snapshot_id.t option) ~(version : int option)
    ~(content_hash : string option) : (unit, string) result =
  (* If either side omitted tokens, we allow it (incremental rollout).
     If both have a value, require equality. *)
  let check_opt (type a) name (pp : a -> string) (a : a option) (b : a option) =
    match (a, b) with
    | Some x, Some y when x <> y ->
        Error
          (Printf.sprintf "Stale %s: expected %s, got %s" name (pp x) (pp y))
    | _ -> Ok ()
  in
  match
    check_opt "snapshot_id" string_of_int (Some entry.snapshot_id) snapshot_id
  with
  | Error _ as e -> e
  | Ok () -> (
      match check_opt "version" string_of_int entry.version version with
      | Error _ as e -> e
      | Ok () -> (
          match
            check_opt "content_hash"
              (fun s -> s)
              entry.content_hash content_hash
          with
          | Error _ as e -> e
          | Ok () -> Ok ()))

let apply_action ~(action_id : Action_id.t) ~(version : int option)
    ~(content_hash : string option) ~(snapshot_id : Snapshot_id.t option) :
    (string, string * string) result =
  match Action_registry.find_opt registry action_id with
  | None -> Error ("E_UNKNOWN_ACTION", Printf.sprintf "Action id: %d" action_id)
  | Some entry -> (
      match validate_tokens ~entry ~snapshot_id ~version ~content_hash with
      | Error msg -> Error ("E_STALE_ACTION", msg)
      | Ok () -> (
          match Snapshot_registry.find_opt snapshots entry.snapshot_id with
          | None -> Error ("E_STALE_ACTION", "No snapshot available for action")
          | Some snap -> (
              let root = Compile.snapshot_root snap in
              match entry.apply root with
              | None ->
                  Error ("E_ACTION_NOT_APPLICABLE", "Action is not applicable")
              | Some new_root ->
                  (* After applying, invalidate old registry; the world changed. *)
                  Action_registry.clear registry;
                  Snapshot_registry.clear snapshots;
                  Ok (Pretty.pp_expr new_root))))

let handle_request (json : J.t) : J.t =
  let id = match JU.member "id" json with `Null -> `Null | v -> v in
  try
    let method_ = JU.member "method" json |> JU.to_string in
    match method_ with
    | "check" ->
        let text = JU.member "text" json |> JU.to_string in
        let version = get_opt_int json "version" in
        let content_hash = get_opt_string json "content_hash" in
        let diags, actions, snapshot_id =
          check_text ~text ~version ~content_hash
        in
        mk_ok_check ~id ~diags ~actions ~snapshot_id ~version ~content_hash
    | "apply_action" -> (
        let action_id = JU.member "action_id" json |> JU.to_int in
        let snapshot_id = get_opt_int json "snapshot_id" in
        let version = get_opt_int json "version" in
        let content_hash = get_opt_string json "content_hash" in
        match apply_action ~action_id ~snapshot_id ~version ~content_hash with
        | Ok new_text -> mk_ok_replace_all ~id ~text:new_text
        | Error (code, message) -> mk_error ~id ~code ~message)
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
