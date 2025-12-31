module Node_id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
end

type t

val create : unit -> t
val fresh : t -> Node_id.t
