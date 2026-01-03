module type S = sig
  type ast_index
  type error
  type diag

  val of_error : ast_index -> error -> diag
end
