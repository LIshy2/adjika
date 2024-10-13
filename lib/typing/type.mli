open Core

type mono =
  | TypeVar of int
  | Int
  | Float
  | Bool
  | Arrow of mono list * mono
  | Named of string
  | Operator of string * mono list
  | MailBox of string
  | Actor of string
[@@deriving compare, sexp]

type poly = Mono of mono | Quant of int list * mono [@@deriving compare, sexp]

val show_mono : mono -> string
val show_poly : poly -> string
val freevars_mono : mono -> (int, Int.comparator_witness) Set.t
val freevars : poly -> (int, Int.comparator_witness) Set.t
val monotype : poly -> mono
val from_decl : Parsing.Ast.TypeDecl.t -> mono
val from_decl_ctx : (string, mono, 'a) Map.t -> Parsing.Ast.TypeDecl.t -> mono
