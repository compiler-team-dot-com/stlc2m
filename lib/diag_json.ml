module Make (Diag : Diag.S) = struct
  module J = Yojson.Safe

  let pos_to_json (p : Lexing.position) : J.t =
    let line = p.pos_lnum in
    let col = p.pos_cnum - p.pos_bol in
    `Assoc [ ("line", `Int line); ("col", `Int col) ]

  let range_to_json (r : Diag.range) : J.t =
    `Assoc
      [ ("start", pos_to_json r.start_pos); ("end", pos_to_json r.end_pos) ]

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
