module type S = sig
  type expr
  type ty
  type node_id

  module StringSet : Set.S

  module Evidence : sig
    type stack_escape = {
      export_id : node_id;
      escaping_vars : string list; (* canonical list, stable order *)
      binders : (string * node_id) list; (* stack var -> binder node id *)
    }
  end

  type error =
    | EUnboundVar of { id : node_id; x : string }
    | EExpectedBool of { id : node_id }
    | ETypeMismatch of { id : node_id; expected : ty; got : ty }
    | EExpectedFun of { id : node_id }
    | EStackEscape of Evidence.stack_escape

  type infer_result = ty * StringSet.t

  val infer : expr -> (infer_result, error) result
end
