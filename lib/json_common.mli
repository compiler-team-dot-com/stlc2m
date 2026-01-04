module type Range = sig
  type t

  val start_pos : t -> Lexing.position
  val end_pos : t -> Lexing.position
end

val pos_to_json : Lexing.position -> Yojson.Safe.t
val range_to_json : (module Range with type t = 'r) -> 'r -> Yojson.Safe.t
