module Make (Snapshot : sig
  type t
end) =
struct
  type t = (Snapshot_id.t, Snapshot.t) Hashtbl.t

  let create () : t = Hashtbl.create 64
  let clear (t : t) : unit = Hashtbl.reset t
  let add (t : t) (id : Snapshot_id.t) (snapshot : Snapshot.t) : unit =
    Hashtbl.replace t id snapshot

  let find_opt (t : t) (id : Snapshot_id.t) : Snapshot.t option =
    Hashtbl.find_opt t id
end
