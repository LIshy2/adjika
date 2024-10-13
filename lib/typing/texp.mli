module Deconstructor : sig
  type t = Var of string | Constructor of string * (t * Type.mono) list
  [@@deriving sexp, compare]

  val show : t -> string
end

module TypedVal : sig
  type 'e t = MonoDef of string * 'e | PolyDef of string * int list * 'e
  [@@deriving sexp, compare]

  val expression : 'e t -> 'e
  val name : 'e t -> string
end

type t =
  | Var of string * Type.mono
  | Const of int * Type.mono
  | Oper of Parsing.Ast.BinOp.t * t * t * Type.mono
  | Block of t TypedVal.t list * t
  | Apply of t * t list * Type.mono
  | PatMatch of t * (Deconstructor.t * t) list * Type.mono
  | Field of t * string * Type.mono
  | Lambda of string list * t * Type.mono
[@@deriving sexp, compare]

val type_of : t -> Type.mono
val show : ?indent:int -> t -> string

module TProgram : sig
  type 'e tfun = {
    name : string;
    arguments : string list;
    fun_type : Type.poly;
    expr : 'e;
  }

  type 'e program = {
    functions : 'e tfun list;
    types : (Type.mono, int) Parsing.Ast.Datatype.t list;
    actors : Type.mono Parsing.Ast.Actor.t list;
    handlers : ('e, 'e TypedVal.t, Type.mono) Parsing.Ast.Handler.t list;
  }

  val show_functions : t program -> string
  val tfun : string -> string list -> Type.poly -> 'a -> 'a tfun
end
