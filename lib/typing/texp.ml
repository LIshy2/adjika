open Parsing
open Core

type expr =
  | Var of string * Type.poly
  | Const of int * Type.poly
  | Oper of Ast.BinOp.t * expr * expr * Type.poly
  | Block of expr Ast.Symbol.val_symbol list * expr
  | Apply of expr * expr list * Type.poly * Type.poly
  | PatMatch of expr * (Ast.Deconstructor.t * expr) list * Type.poly
  | Field of expr * string * Type.poly
  | Lambda of (string * Type.poly) list * string list * expr * Type.poly

let rec type_of = function
  | Var (_, t) -> t
  | Const (_, t) -> t
  | Oper (_, _, _, t) -> t
  | Block (_, result) -> type_of result
  | Field (_, _, t) -> t
  | PatMatch (_, _, t) -> t
  | Lambda (_, _, _, t) -> t
  | Apply (_, _, t, _) -> t

let rec show ?(indent = 0) exp =
  match exp with
  | Var (name, _) -> name
  | Const (value, _) -> string_of_int value
  | Oper (op, lhs, rhs, _) ->
      let op_sym = match op with Plus -> "+" | Minus -> "-" | Mult -> "*" in
      show lhs ^ " " ^ op_sym ^ " " ^ show rhs
  | Block (defs, result) ->
      let show_def Ast.Symbol.{ name; result } =
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
  | Lambda (_, arguments, body, Mono (Arrow (args, result))) ->
      let typed_args = List.zip_exn arguments args in
      let arg_strings =
        List.map typed_args ~f:(fun (name, tp) ->
            name ^ ": " ^ Type.show_mono tp)
      in
      "fun("
      ^ String.concat ~sep:", " arg_strings
      ^ ") -> " ^ Type.show_mono result ^ " = " ^ show body
  | Lambda (_, arguments, body, Quant (_, Arrow (args, result))) ->
      let typed_args = List.zip_exn arguments args in
      let arg_strings =
        List.map typed_args ~f:(fun (name, tp) ->
            name ^ ": " ^ Type.show_mono tp)
      in
      "fun("
      ^ String.concat ~sep:", " arg_strings
      ^ ") -> " ^ Type.show_mono result ^ " = " ^ show body
  | Apply (fun_val, arg_vals, _, _) ->
      let arg_string = List.map arg_vals ~f:show in
      show fun_val ^ "(" ^ String.concat ~sep:", " arg_string ^ ")"
  | _ ->
      print_endline "AAAAA";
      exit (-1)

let rec generealize ctx = function
  | Var (name, t) -> Var (name, Tenv.generealize ctx (Type.monotype t))
  | Const (v, t) -> Const (v, Tenv.generealize ctx (Type.monotype t))
  | Oper (op, lhs, rhs, t) ->
      Oper (op, lhs, rhs, Tenv.generealize ctx (Type.monotype t))
  | Block (defs, result) -> Block (defs, generealize ctx result)
  | Lambda (captures, args, body, t) ->
      Lambda (captures, args, body, Tenv.generealize ctx (Type.monotype t))
  | Field (str, field, t) ->
      Field (str, field, Tenv.generealize ctx (Type.monotype t))
  | PatMatch (obj, cases, t) ->
      PatMatch (obj, cases, Tenv.generealize ctx (Type.monotype t))
  | Apply (fun_val, args_val, t, at) ->
      Apply (fun_val, args_val, Tenv.generealize ctx (Type.monotype t), at)

module TypedProgram = struct
  type typed_fun = {
    name : string;
    arguments : string list;
    fun_type : Type.poly;
    expr : expr;
  }

  type t = {
    functions : typed_fun list;
    types : Type.mono Ast.Symbol.type_symbol list;
  }
end
