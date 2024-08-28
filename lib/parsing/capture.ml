open Ast
open Core
open Result.Let_syntax

module Cex = struct
  type capture_expr =
    | Var of string
    | Const of int
    | Oper of BinOp.t * capture_expr * capture_expr
    | Block of capture_expr Symbol.val_symbol list * capture_expr
    | Field of capture_expr * string
    | Apply of capture_expr * capture_expr list
    | PatMatch of capture_expr * (Deconstructor.t * capture_expr) list
    | Lambda of string list * string list * capture_expr

  let rec show ?(indent = 0) = function
    | Var name -> name
    | Const value -> string_of_int value
    | Oper (op, lhs, rhs) ->
        let op_sym = match op with Plus -> "+" | Minus -> "-" | Mult -> "*" in
        show lhs ^ " " ^ op_sym ^ " " ^ show rhs
    | Block (defs, result) ->
        let show_def Symbol.{ name; result } =
          String.make ((indent + 1) * 4) ' '
          ^ name ^ " = "
          ^ show ~indent:(indent + 1) result
        in
        "{\n"
        ^ String.concat ~sep:"\n" (List.map defs ~f:show_def)
        ^ "\n"
        ^ String.make ((indent + 1) * 4) ' '
        ^ show result ^ "\n"
        ^ String.make (indent * 4) ' '
        ^ "}"
    | Field (str, name) -> show ~indent str ^ "." ^ name
    | PatMatch (_, _) -> "match"
    | Lambda (_, arguments, body) ->
        "fun(" ^ String.concat ~sep:", " arguments ^ ")" ^ " = " ^ show body
    | Apply (fun_val, arg_vals) ->
        let arg_string = List.map arg_vals ~f:show in
        show fun_val ^ "(" ^ String.concat ~sep:", " arg_string ^ ")"
end

module NameContext = struct
  type t = {
    toplevels : (string, String.comparator_witness) Set.t;
    locals : (string, String.comparator_witness) Set.t;
    fields : (string, String.comparator_witness) Set.t;
  }

  let name_check self name =
    Set.mem self.toplevels name || Set.mem self.locals name

  let local_check self name = Set.mem self.locals name
  let field_check self name = Set.mem self.fields name
  let add_local self name = { self with locals = Set.add self.locals name }

  let add_locals self names =
    {
      self with
      locals = Set.union self.locals (Set.of_list (module String) names);
    }

  let init toplevels initial fields =
    {
      toplevels = Set.of_list (module String) toplevels;
      locals = Set.of_list (module String) initial;
      fields = Set.of_list (module String) fields;
    }
end

type name_check_error =
  | UnknownName of string * NameContext.t
  | UsedName of string

let capture_ast_expr toplevels initials fields expr =
  let rec check nctx = function
    | Expr.Var name ->
        let exists = NameContext.name_check nctx name in
        if exists then Result.Ok (Cex.Var name)
        else Result.Error (UnknownName (name, nctx))
    | Expr.Const value -> Result.Ok (Cex.Const value)
    | Expr.Oper (op, lhs, rhs) ->
        let%bind clhs = check nctx lhs in
        let%bind crhs = check nctx rhs in
        Result.Ok (Cex.Oper (op, clhs, crhs))
    | Expr.Apply (fun_val, arguments) ->
        let%bind cfun = check nctx fun_val in
        let%bind cargs =
          List.fold_right arguments ~init:(Result.Ok []) ~f:(fun x acc ->
              let%bind prev = acc in
              let%bind current = check nctx x in
              Result.Ok (current :: prev))
        in
        Result.Ok (Cex.Apply (cfun, cargs))
    | Expr.Block (defs, result) ->
        let%bind rev_cdefs, block_ctx =
          List.fold_left defs
            ~init:(Result.Ok ([], nctx))
            ~f:(fun acc Symbol.{ name; result } ->
              let%bind prev, block_context = acc in
              if not (NameContext.name_check block_context name) then
                let%bind current = check block_context result in
                Result.Ok
                  ( Symbol.{ name; result = current } :: prev,
                    NameContext.add_local block_context name )
              else Result.Error (UsedName name))
        in
        let%bind cresult = check block_ctx result in
        Result.Ok (Cex.Block (List.rev rev_cdefs, cresult))
    | Expr.Lambda (arguments, result) ->
        let%bind lambda_ctx =
          List.fold_left arguments ~init:(Result.Ok nctx) ~f:(fun acc arg ->
              let%bind ctx = acc in
              if not (NameContext.name_check ctx arg) then
                Result.Ok (NameContext.add_local ctx arg)
              else Result.Error (UsedName arg))
        in
        let%bind cresult = check lambda_ctx result in
        let rec caught = function
          | Expr.Var name ->
              if NameContext.local_check nctx name then [ name ] else []
          | Expr.Oper (_, lhs, rhs) -> List.append (caught lhs) (caught rhs)
          | Expr.Apply (fun_val, arguments) ->
              let arguments_capture =
                List.concat (List.map arguments ~f:caught)
              in
              let fun_capture = caught fun_val in
              List.append arguments_capture fun_capture
          | Expr.Lambda (_, lambda_expression) -> caught lambda_expression
          | Expr.Block (defs, result) ->
              let def_capture =
                List.concat
                  (List.map defs ~f:(fun symbol -> caught symbol.result))
              in
              let result_capture = caught result in
              List.append def_capture result_capture
          | _ -> []
        in
        let lambda_catch = caught result in
        Result.Ok (Cex.Lambda (lambda_catch, arguments, cresult))
    | Expr.PatMatch (obj, cases) ->
        let%bind cobj = check nctx obj in
        let%bind ccases =
          List.fold_right cases ~init:(Result.Ok []) ~f:(fun (d, e) acc ->
              let rec names c =
                match c with
                | Deconstructor.Var name -> [ name ]
                | Deconstructor.Constructor (_, deconstructors) ->
                    List.concat_map deconstructors ~f:(fun d -> names d)
              in
              let case_name_context = NameContext.add_locals nctx (names d) in
              let%bind res = check case_name_context e in
              let%map acc = acc in
              (d, res) :: acc)
        in
        Result.Ok (Cex.PatMatch (cobj, ccases))
    | Expr.Field (str, name) ->
        let%bind cstr = check nctx str in
        let%bind cname =
          if NameContext.field_check nctx name then Result.Ok name
          else Result.Error (UnknownName (name, nctx))
        in
        Result.Ok (Cex.Field (cstr, cname))
  in
  check (NameContext.init toplevels initials fields) expr

(* todo function and fields ambitius definiton *)
let capture_ast_program program =
  let open Ast.Program in
  let toplevel_names =
    let function_names =
      List.map program.functions ~f:(fun fun_symbol -> fun_symbol.name)
    in
    let constructor_names =
      List.bind program.types ~f:(fun type_symbol ->
          Symbol.constructors_names type_symbol)
    in
    List.append function_names constructor_names
  in
  let field_names =
    List.bind program.types ~f:(fun type_symbol ->
        List.bind ~f:Symbol.field_names type_symbol.constructors)
  in
  let%bind cfuncs =
    List.fold_left program.functions ~init:(Result.Ok [])
      ~f:(fun acc fun_symbol ->
        let%bind acc_list = acc in
        let%map captured_body =
          capture_ast_expr toplevel_names fun_symbol.arguments field_names
            fun_symbol.result
        in
        let fun_expression =
          Symbol.
            {
              name = fun_symbol.name;
              arguments = fun_symbol.arguments;
              result = captured_body;
            }
        in
        fun_expression :: acc_list)
  in
  Result.Ok { functions = cfuncs; types = program.types }
