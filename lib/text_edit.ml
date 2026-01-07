module J = Yojson.Safe

type pos = { line : int; col : int } (* line is 1-based, col is 0-based *)
type range = { start_ : pos; end_ : pos }
type t = { range : range; replacement : string }

let pos_to_json (p : pos) : J.t =
  `Assoc [ ("line", `Int p.line); ("col", `Int p.col) ]

let range_to_json (r : range) : J.t =
  `Assoc [ ("start", pos_to_json r.start_); ("end", pos_to_json r.end_) ]

let to_json (e : t) : J.t =
  `Assoc
    [ ("range", range_to_json e.range); ("replacement", `String e.replacement) ]
