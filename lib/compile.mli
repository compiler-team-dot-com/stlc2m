open Ast
open Checker

type snapshot

type t = {
  snapshot : snapshot option;
  result : (ty * StringSet.t, error) result option;
}

type parse_error =
  | LexError of { msg : string; pos : Lexing.position }
  | ParseError of { pos : Lexing.position }

val from_string :
  ?version:int -> ?fname:string -> string -> (t, parse_error) result

val from_channel :
  ?version:int -> ?fname:string -> in_channel -> (t, parse_error) result

val diag_of_error : snapshot -> error -> Diag.t
