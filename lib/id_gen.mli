module type Id = sig
  type t

  val compare : t -> t -> int
  val to_int : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

module type S = sig
  module Node_id : Id

  type t

  val create : unit -> t
  val fresh : t -> Node_id.t

  module NodeIdMap : Map.S with type key = Node_id.t
  module NodeIdSet : Set.S with type elt = Node_id.t
  module NodeIdHashtbl : Hashtbl.S with type key = Node_id.t
end

module Make () : S
