module type S = sig
  type expr
  type ty
  type range
  type node_id
  type ast_index
  type diag

  module StringSet : Set.S

  module Evidence : sig
    type stack_escape = {
      export_id : node_id;
      _escaping_vars : string list; (* canonical list, stable order *)
      binders : (string * node_id) list; (* stack var -> binder node id *)
    }
  end

  type error =
    | EUnboundVar of { range : range; x : string }
    | EExpectedBool of { range : range }
    | ETypeMismatch of { range : range; expected : ty; got : ty }
    | EExpectedFun of { range : range }
    | EStackEscape of Evidence.stack_escape

  type infer_result = ty * StringSet.t

  val infer : expr -> (infer_result, error) result
  val diag_of_error : ast_index -> error -> diag
end
