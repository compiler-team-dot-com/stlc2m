module type S = sig
  type node_id
  type ty
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

  val error :
    ?related:related list -> code:string -> primary:node_id -> payload -> t
end
