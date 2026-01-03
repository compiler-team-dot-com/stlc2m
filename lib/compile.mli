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
