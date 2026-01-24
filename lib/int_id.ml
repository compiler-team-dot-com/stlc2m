module type S = Int_id_intf.S

module Make
    (X : sig
      val name : string
    end)
    () : S = struct
  type t = int

  let of_int (x : int) : t option = if x >= 0 then Some x else None

  let of_int_exn (x : int) : t =
    match of_int x with
    | Some t -> t
    | None -> invalid_arg (X.name ^ ".of_int_exn: negative id")

  let to_int (x : t) : int = x
  let compare (a : t) (b : t) : int = Int.compare a b
end
