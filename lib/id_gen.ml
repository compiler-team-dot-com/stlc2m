module Node_id = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let hash = Int.hash
  let to_string = string_of_int
end

type t = { mutable next : Node_id.t }

let create () = { next = 0 }

let fresh g =
  let id = g.next in
  g.next <- g.next + 1;
  id
