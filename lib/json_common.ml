module J = Yojson.Safe

module type Range = sig
  type t

  val start_pos : t -> Lexing.position
  val end_pos : t -> Lexing.position
end

let pos_to_json (p : Lexing.position) : J.t =
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  `Assoc [ ("line", `Int line); ("col", `Int col) ]

let range_to_json (type r) (module R : Range with type t = r) (x : r) : J.t =
  `Assoc
    [
      ("start", pos_to_json (R.start_pos x)); ("end", pos_to_json (R.end_pos x));
    ]
