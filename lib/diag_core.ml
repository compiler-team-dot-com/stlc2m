module type S = Diag_core_intf.S

module Make (Ast : Ast.S) = struct
  type node_id = Ast.node_id
  type ty = Ast.ty
  type severity = Error | Warning | Info
  type related = Stack_binder of { x : string; binder_id : node_id }

  type payload =
    | Unbound_var of { x : string }
    | Expected_bool
    | Type_mismatch of { expected : ty; got : ty }
    | Expected_fun
    | Stack_escape of { escaping_vars : string list }

  type t = {
    code : string;
    severity : severity;
    primary : node_id;
    payload : payload;
    related : related list;
  }

  let error ?(related = []) ~code ~primary payload : t =
    { code; severity = Error; primary; payload; related }
end
