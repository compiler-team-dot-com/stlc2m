module type S = sig
  type t

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val compare : t -> t -> int
end
