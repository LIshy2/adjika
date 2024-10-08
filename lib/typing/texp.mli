module Deconstructor : sig
  type t = Var of string | Constructor of string * (t * Type.mono) list

  val show : t -> string
end

type t =
  | Var of string * Type.poly
  | Const of int * Type.poly
  | Oper of Parsing.Ast.BinOp.t * t * t * Type.poly
  | Block of t Parsing.Ast.Val.t list * t
  | Apply of t * t list * Type.poly
  | PatMatch of t * (Deconstructor.t * t) list * Type.poly
  | Field of t * string * Type.poly
  | Lambda of string list * t * Type.poly

val type_of : t -> Type.poly
val show : ?indent:int -> t -> string
val generealize : Tenv.t -> t -> t

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
    handlers : ('e, Type.mono) Parsing.Ast.Handler.t list;
  }

  val show_functions : t program -> string
  val tfun : string -> string list -> Type.poly -> 'a -> 'a tfun
end
