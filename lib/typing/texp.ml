open Parsing
open Core

module Deconstructor = struct
  type t = Var of string | Constructor of string * (t * Type.mono) list
  [@@deriving sexp, compare]

  let rec show = function
    | Var name -> name
    | Constructor (name, subs) ->
        let inner =
          List.map subs ~f:(fun (dec, t) -> show dec ^ ":" ^ Type.show_mono t)
        in
        name ^ "(" ^ String.concat ~sep:", " inner ^ ")"
end

module TypedVal = struct
  type 'e t = MonoDef of string * 'e | PolyDef of string * int list * 'e
  [@@deriving sexp, compare]

  let expression = function
    | MonoDef (_, expr) -> expr
    | PolyDef (_, _, expr) -> expr

  let name = function MonoDef (name, _) -> name | PolyDef (name, _, _) -> name
end

type t =
  | Var of string * Type.mono
  | Const of int * Type.mono
  | Oper of Ast.BinOp.t * t * t * Type.mono
  | Block of t TypedVal.t list * t
  | Apply of t * t list * Type.mono
  | PatMatch of t * (Deconstructor.t * t) list * Type.mono
  | Field of t * string * Type.mono
  | Lambda of string list * t * Type.mono
[@@deriving sexp, compare]

let rec type_of = function
  | Var (_, t) -> t
  | Const (_, t) -> t
  | Oper (_, _, _, t) -> t
  | Block (_, result) -> type_of result
  | Field (_, _, t) -> t
  | PatMatch (_, _, t) -> t
  | Lambda (_, _, t) -> t
  | Apply (_, _, t) -> t

let rec show ?(indent = 0) exp =
  match exp with
  | Var (name, _) -> name
  | Const (value, _) -> string_of_int value
  | Oper (op, lhs, rhs, _) ->
      let op_sym = match op with Plus -> "+" | Minus -> "-" | Mult -> "*" in
      show lhs ^ " " ^ op_sym ^ " " ^ show rhs
  | Block (defs, result) ->
      let show_def = function
        | TypedVal.MonoDef (name, result) ->
            String.make ((indent + 1) * 4) ' '
            ^ name ^ ": "
            ^ Type.show_mono (type_of result)
            ^ " = "
            ^ show ~indent:(indent + 1) result
        | TypedVal.PolyDef (name, quants, result) ->
            String.make ((indent + 1) * 4) ' '
            ^ name ^ ": "
            ^ Type.show_poly (Quant (quants, type_of result))
            ^ " = "
            ^ show ~indent:(indent + 1) result
      in
      "{\n"
      ^ String.concat ~sep:"\n" (List.map defs ~f:show_def)
      ^ "\n"
      ^ String.make ((indent + 1) * 4) ' '
      ^ show result ^ "\n"
      ^ String.make (indent * 4) ' '
      ^ "}"
  | Lambda (arguments, body, Arrow (args, result)) ->
      let typed_args = List.zip_exn arguments args in
      let arg_strings =
        List.map typed_args ~f:(fun (name, tp) ->
            name ^ ": " ^ Type.show_mono tp)
      in
      "fun("
      ^ String.concat ~sep:", " arg_strings
      ^ ") -> " ^ Type.show_mono result ^ " = " ^ show body
  | Apply (fun_val, arg_vals, t) ->
      let arg_string = List.map arg_vals ~f:show in
      show fun_val ^ "("
      ^ String.concat ~sep:", " arg_string
      ^ "): " ^ Type.show_mono t
  | PatMatch (obj, deconstructors, _) ->
      "match " ^ show obj ^ " {\n"
      ^ String.concat ~sep:"\n"
          (List.map deconstructors ~f:(fun (dec, expr) ->
               let line =
                 Deconstructor.show dec ^ " => "
                 ^ show ~indent:(indent + 1) expr
               in
               String.make ((indent + 1) * 4) ' ' ^ line))
      ^ "\n}"
  | Field (obj, name, _) -> show obj ^ "." ^ name
  | Lambda (_, _, t) -> "!!!wrong lambda type!!!" ^ Type.show_mono t

module TProgram = struct
  type 'e tfun = {
    name : string;
    arguments : string list;
    fun_type : Type.poly;
    expr : 'e;
  }

  type 'e program = {
    functions : 'e tfun list;
    types : (Type.mono, int) Ast.Datatype.t list;
    actors : Type.mono Ast.Actor.t list;
    handlers : ('e, 'e TypedVal.t, Type.mono) Ast.Handler.t list;
  }

  let show_functions program =
    String.concat ~sep:"\n\n"
      (List.map program.functions ~f:(fun { name; arguments; fun_type; expr } ->
           "fun " ^ name ^ "[" ^ Type.show_poly fun_type ^ "]" ^ "("
           ^ String.concat ~sep:", " arguments
           ^ ")" ^ "=" ^ show expr))

  let tfun name arguments fun_type expr = { name; arguments; fun_type; expr }
end
