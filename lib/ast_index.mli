open Ast
open Id_gen

type t

exception Bug of string

val build : expr -> t
val expr : t -> Node_id.t -> expr
val range : t -> Node_id.t -> range
val parent : t -> Node_id.t -> Node_id.t option
