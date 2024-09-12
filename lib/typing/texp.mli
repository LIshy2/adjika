module Deconstructor :
  sig
    type t =
        Var of string
      | Constructor of string * (t * Typing.Type.mono) list
    val show : t -> string
  end
type t =
    Var of string * Typing.Type.poly
  | Const of int * Typing.Type.poly
  | Oper of Parsing.Ast.BinOp.t * t * t * Typing.Type.poly
  | Block of t Parsing.Ast.Val.t list * t
  | Apply of t * t list * Typing.Type.poly
  | PatMatch of t * (Deconstructor.t * t) list * Typing.Type.poly
  | Field of t * string * Typing.Type.poly
  | Lambda of string list * t * Typing.Type.poly

val type_of : t -> Typing.Type.poly
val show : ?indent:int -> t -> string
val generealize : Tenv.t -> t -> t

module TProgram :
  sig
    type 'e tfun = {
      name : string;
      arguments : string list;
      fun_type : Typing.Type.poly;
      expr : 'e;
    }
    type 'e program = {
      functions : 'e tfun list;
      types : (Typing.Type.mono, int) Parsing.Ast.Datatype.t list;
      actors : Typing.Type.mono Parsing.Ast.Actor.t list;
      handlers : ('e, Typing.Type.mono) Parsing.Ast.Handler.t list;
    }

    val show_functions : t program -> string
    val tfun : string -> string list -> Typing.Type.poly -> 'a -> 'a tfun
  end
