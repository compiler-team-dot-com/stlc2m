module type S = sig
  module Range : Ast.Range

  type range = Range.t
  type severity = Error | Warning | Info
  type related = { range : range; message : string }

  type t = {
    code : string;
    message : string;
    severity : severity;
    range : range;
    related : related list;
  }

  val error : ?related:related list -> code:string -> range:range -> string -> t
  val to_human : ?file:string -> t -> string
  val severity_to_string : severity -> string
end
