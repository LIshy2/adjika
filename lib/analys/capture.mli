open Typing

module Cexp : sig
  type t =
    | Var of string * Type.mono
    | Const of int * Type.mono
    | Oper of Parsing.Ast.BinOp.t * t * t * Type.mono
    | Block of t Texp.TypedVal.t list * t
    | Field of t * string * Type.mono
    | Apply of t * t list * Type.mono
    | PatMatch of t * (Texp.Deconstructor.t * t) list * Type.mono
    | Lambda of (string * Type.mono) list * string list * t * Type.mono

  val show : ?indent:int -> t -> string
  val type_of : t -> Type.mono
end

type name_check_error = UnknownName of string | UsedName of string

val capture_program :
  Texp.t Texp.TProgram.program ->
  (Cexp.t Texp.TProgram.program, name_check_error) result
