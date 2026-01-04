module type S = sig
  type error
  type diag_core

  val of_error : error -> diag_core
end
