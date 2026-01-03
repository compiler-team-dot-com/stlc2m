module type S = sig
  type range
  type t
  type expr
  type node_id

  val build : expr -> t

  val range : t -> node_id -> range
  val parent : t -> node_id -> node_id option
end
