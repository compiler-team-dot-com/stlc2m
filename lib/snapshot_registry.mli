module Make (Snapshot : sig
  type t
end) : sig
  type t

  val create : unit -> t
  val clear : t -> unit
  val add : t -> Snapshot_id.t -> Snapshot.t -> unit
  val find_opt : t -> Snapshot_id.t -> Snapshot.t option
end
