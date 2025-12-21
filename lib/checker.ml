open Ast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type stack_binding = { ty : ty; binder_range : range }
type env = { heap : ty StringMap.t; stack : stack_binding StringMap.t }

let empty_env : env = { heap = StringMap.empty; stack = StringMap.empty }

let lookup (env : env) (x : string) :
    [ `Heap of ty | `Stack of stack_binding ] option =
  match StringMap.find_opt x env.stack with
  | Some b -> Some (`Stack b)
  | None -> (
      match StringMap.find_opt x env.heap with
      | Some t -> Some (`Heap t)
      | None -> None)

let extend_heap (env : env) (x : string) (t : ty) : env =
  { env with heap = StringMap.add x t env.heap }

let extend_stack (env : env) (x : string) (b : stack_binding) : env =
  { env with stack = StringMap.add x b env.stack }

type infer_reslt = ty * StringSet.t

let union_deps (a : StringSet.t) (b : StringSet.t) = StringSet.union a b
