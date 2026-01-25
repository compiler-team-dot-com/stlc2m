module Ast : Ast.S
module Diag : Diag.S

type parse_error =
  | LexError of { msg : string; pos : Lexing.position }
  | ParseError of { pos : Lexing.position }

type snapshot
type error
type infer_result

type t = {
  snapshot : snapshot option;
  result : (infer_result, error) result option;
}

val from_string :
  ?version:int -> ?fname:string -> string -> (t, parse_error) result

val from_channel :
  ?version:int -> ?fname:string -> in_channel -> (t, parse_error) result

val diag_of_error : snapshot -> error -> Diag.t

type action_kind = Action_kind.t

type action = {
  id : Action_id.t;
  kind : action_kind;
  title : string;
  (* Semantic targets (node_ids) are included for future “apply action” messages. *)
  targets : Ast.node_id list;
  (* Derived UI highlights (ranges). *)
  highlights : Ast.range list;
  rationale : string;
}

val snapshot_root : snapshot -> Ast.expr

type action_impl = Ast.expr -> Ast.expr option

val report_of_error :
  snapshot -> error -> Diag.t * action list * (Action_id.t * action_impl) list

val report_ok : snapshot -> action list * (Action_id.t * action_impl) list
