open Parsing
open Core

module Deconstructor = struct
  type t = Var of string | Constructor of string * (t * Type.mono) list

  let rec show = function
    | Var name -> name
    | Constructor (name, subs) ->
        let inner =
          List.map subs ~f:(fun (dec, t) -> show dec ^ ":" ^ Type.show_mono t)
        in
        name ^ "(" ^ String.concat ~sep:", " inner ^ ")"
end

type t =
  | Var of string * Type.poly
  | Const of int * Type.poly
  | Oper of Ast.BinOp.t * t * t * Type.poly
  | Block of t Ast.Val.t list * t
  | Apply of t * t list * Type.poly
  | PatMatch of t * (Deconstructor.t * t) list * Type.poly
  | Field of t * string * Type.poly
  | Lambda of string list * t * Type.poly

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
      let show_def Ast.Val.{ name; result } =
        String.make ((indent + 1) * 4) ' '
        ^ name ^ ": "
        ^ Type.show_poly (type_of result)
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
  | Lambda (arguments, body, Mono (Arrow (args, result))) ->
      let typed_args = List.zip_exn arguments args in
      let arg_strings =
        List.map typed_args ~f:(fun (name, tp) ->
            name ^ ": " ^ Type.show_mono tp)
      in
      "fun("
      ^ String.concat ~sep:", " arg_strings
      ^ ") -> " ^ Type.show_mono result ^ " = " ^ show body
  | Lambda (arguments, body, Quant (_, Arrow (args, result))) ->
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
      ^ "): " ^ Type.show_poly t
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
  | Lambda (_, _, t) -> "!!!wrong lambda type!!!" ^ Type.show_poly t

let rec generealize ctx = function
  | Var (name, t) -> Var (name, Tenv.generealize ctx (Type.monotype t))
  | Const (v, t) -> Const (v, Tenv.generealize ctx (Type.monotype t))
  | Oper (op, lhs, rhs, t) ->
      Oper (op, lhs, rhs, Tenv.generealize ctx (Type.monotype t))
  | Block (defs, result) -> Block (defs, generealize ctx result)
  | Lambda (args, body, t) ->
      Lambda (args, body, Tenv.generealize ctx (Type.monotype t))
  | Field (str, field, t) ->
      Field (str, field, Tenv.generealize ctx (Type.monotype t))
  | PatMatch (obj, cases, t) ->
      PatMatch (obj, cases, Tenv.generealize ctx (Type.monotype t))
  | Apply (fun_val, args_val, t) ->
      Apply (fun_val, args_val, Tenv.generealize ctx (Type.monotype t))

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
    handlers : ('e, Type.mono) Ast.Handler.t list;
  }

  let show_functions program =
    String.concat ~sep:"\n\n"
      (List.map program.functions ~f:(fun { name; arguments; fun_type; expr } ->
           "fun " ^ name ^ "[" ^ Type.show_poly fun_type ^ "]" ^ "("
           ^ String.concat ~sep:", " arguments
           ^ ")" ^ "=" ^ show expr))

  let tfun name arguments fun_type expr = { name; arguments; fun_type; expr }
end
