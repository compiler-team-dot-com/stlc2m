module Make (Diag : Diag.S) : sig
  val diag_to_json : Diag.t -> Yojson.Safe.t
end
