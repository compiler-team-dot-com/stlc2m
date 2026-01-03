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

module Make () : S = struct
  module Node_id = struct
    type t = int

    let compare = Int.compare
    let equal (a : t) b = a = b
    let hash (x : t) = Hashtbl.hash x
    let to_int x = x
    let to_string = string_of_int
    let pp fmt x = Format.fprintf fmt "%d" x
  end

  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh g =
    let id = g.next in
    g.next <- id + 1;
    id

  module NodeIdMap = Map.Make (Node_id)
  module NodeIdSet = Set.Make (Node_id)
  module NodeIdHashtbl = Hashtbl.Make (Node_id)
end
