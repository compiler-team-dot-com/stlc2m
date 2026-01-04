module Ast = Compile.Ast
module J = Yojson.Safe
module JC = Json_common

module Range : Json_common.Range with type t = Ast.range = struct
  type t = Ast.range

  let start_pos (r : t) = r.start_pos
  let end_pos (r : t) = r.end_pos
end

let range_to_json (r : Ast.range) : J.t = JC.range_to_json (module Range) r

let node_id_to_json (id : Ast.node_id) : J.t =
  `String (Ast.string_of_node_id id)

let action_kind_to_json (k : Compile.action_kind) : J.t =
  match k with Quickfix -> `String "quickfix" | Explain -> `String "explain"

let action_to_json (a : Compile.action) : J.t =
  `Assoc
    [
      ("id", `Int a.id);
      ("kind", action_kind_to_json a.kind);
      ("title", `String a.title);
      ("targets", `List (List.map node_id_to_json a.targets));
      ("highlights", `List (List.map range_to_json a.highlights));
      ("rationale", `String a.rationale);
    ]
