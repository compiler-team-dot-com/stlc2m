module Make (Ast : Ast.S) = struct
  type entry = {
    apply : Ast.expr -> Ast.expr option;
    version : int option;
    content_hash : string option;
    snapshot_id : Snapshot_id.t;
  }

  type t = (Action_id.t, entry) Hashtbl.t

  let create () : t = Hashtbl.create 64
  let clear (t : t) : unit = Hashtbl.reset t

  let add (t : t) (id : Action_id.t) (entry : entry) : unit =
    Hashtbl.replace t id entry

  let find_opt (t : t) (id : Action_id.t) : entry option = Hashtbl.find_opt t id
end
