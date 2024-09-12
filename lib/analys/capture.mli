module Cexp : sig
  type t =
    | Var of string * Typing.Type.poly
    | Const of int * Typing.Type.poly
    | Oper of Parsing.Ast.BinOp.t * t * t * Typing.Type.poly
    | Block of t Parsing.Ast.Val.t list * t
    | Field of t * string * Typing.Type.poly
    | Apply of t * t list * Typing.Type.poly
    | PatMatch of t * (Typing.Texp.Deconstructor.t * t) list * Typing.Type.poly
    | Lambda of
        (string * Typing.Type.poly) list * string list * t * Typing.Type.poly

  val show : ?indent:int -> t -> string
  val type_of : t -> Typing.Type.poly
end

type name_check_error = UnknownName of string | UsedName of string

val capture_program :
  Typing.Texp.expr Typing.Texp.TProgram.t ->
  (Cexp.t Typing.Texp.TProgram.t, name_check_error) result
