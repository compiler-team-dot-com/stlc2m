module Make (Ast : Ast.S) : sig
  type entry = {
    apply : Ast.expr -> Ast.expr option;
    version : int option;
    content_hash : string option;
    snapshot_id : Snapshot_id.t;
  }

  type t

  val create : unit -> t
  val clear : t -> unit
  val add : t -> Action_id.t -> entry -> unit
  val find_opt : t -> Action_id.t -> entry option
end
