module Make (Diag : Diag.S) = struct
  module J = Yojson.Safe
  module JC = Json_common

  module Range : Json_common.Range with type t = Diag.range = struct
    type t = Diag.range

    let start_pos (r : t) = r.start_pos
    let end_pos (r : t) = r.end_pos
  end

  let range_to_json (r : Diag.range) : J.t = JC.range_to_json (module Range) r

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
end
