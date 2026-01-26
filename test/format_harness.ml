open Stlc2m
module Ast = Compile.Ast
module Pretty = Pretty.Make (Ast)

let read_all (path : string) : string =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> In_channel.input_all ic)

let parse_root_exn ~(path : string) (src : string) : Ast.expr =
  match Compile.from_string ~version:0 ~fname:path src with
  | Error _ -> failwith @@ "Parse error in " ^ path
  | Ok { snapshot = None; _ } -> failwith @@ "Empty program in " ^ path
  | Ok { snapshot = Some snap; _ } -> Compile.snapshot_root snap

(* Normalized "shape" for equality, erasing ids and ranges. *)
type sexpr =
  | SVar of string
  | SInt of int
  | SBool of bool
  | SIf of sexpr * sexpr * sexpr
  | SLam of string * Ast.ty * sexpr
  | SApp of sexpr * sexpr
  | SLet of string * sexpr * sexpr
  | SLetStack of string * sexpr * sexpr
  | SExport of sexpr

let rec shape (e : Ast.expr) : sexpr =
  match e.node with
  | EVar x -> SVar x
  | EInt n -> SInt n
  | EBool b -> SBool b
  | EIf (c, tbr, fbr) -> SIf (shape c, shape tbr, shape fbr)
  | ELam (x, _xr, xty, body) -> SLam (x, xty, shape body)
  | EApp (f, a) -> SApp (shape f, shape a)
  | ELet (x, _xr, e1, e2) -> SLet (x, shape e1, shape e2)
  | ELetStack { x; e1; e2; _ } -> SLetStack (x, shape e1, shape e2)
  | EExport e1 -> SExport (shape e1)

let check_file (path : string) : unit =
  let src = read_all path in

  (* Parse -> pretty *)
  let e1 = parse_root_exn ~path src in
  let p1 = Pretty.pp_expr e1 in

  (* Pretty -> parse -> pretty *)
  let e2 = parse_root_exn ~path p1 in
  let p2 = Pretty.pp_expr e2 in

  (* 1) Idempotence *)
  if p1 <> p2 then
    failwith
    @@ Printf.sprintf
         "Pretty not idempotent for %s\n--- p1 ---\n%s\n--- p2 ---\n%s\n" path
         p1 p2;

  (* 2) Reparse stability (module ids/ranges) *)
  if shape e1 <> shape e2 then
    failwith
    @@ Printf.sprintf "Pretty reparses to different AST shape for %s\n" path

let () =
  let files =
    Array.to_list Sys.argv |> List.tl
    (* drop argv[0] *)
  in
  if files = [] then (
    prerr_endline "format_harness: expected .lang file arguments";
    exit 2);

  List.iter
    (fun f ->
      prerr_endline @@ "[TEST] format_harness " ^ f;
      check_file f)
    files
