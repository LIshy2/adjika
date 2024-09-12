open Typing

module Cexp : sig
  type t =
    | Var of string * Type.poly
    | Const of int * Type.poly
    | Oper of Parsing.Ast.BinOp.t * t * t * Type.poly
    | Block of t Parsing.Ast.Val.t list * t
    | Field of t * string * Type.poly
    | Apply of t * t list * Type.poly
    | PatMatch of t * (Texp.Deconstructor.t * t) list * Type.poly
    | Lambda of (string * Type.poly) list * string list * t * Type.poly

  val show : ?indent:int -> t -> string
  val type_of : t -> Type.poly
end

type name_check_error = UnknownName of string | UsedName of string

val capture_program :
  Texp.t Texp.TProgram.program ->
  (Cexp.t Texp.TProgram.program, name_check_error) result
