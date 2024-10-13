open Parsing
open Typing
open Core
open Result.Let_syntax

module Cexp = struct
  type t =
    | Var of string * Type.mono
    | Const of int * Type.mono
    | Oper of Ast.BinOp.t * t * t * Type.mono
    | Block of t Texp.TypedVal.t list * t
    | Field of t * string * Type.mono
    | Apply of t * t list * Type.mono
    | PatMatch of t * (Texp.Deconstructor.t * t) list * Type.mono
    | Lambda of (string * Type.mono) list * string list * t * Type.mono

  let rec show ?(indent = 0) = function
    | Var (name, _) -> name
    | Const (value, _) -> string_of_int value
    | Oper (op, lhs, rhs, _) ->
        let op_sym = match op with Plus -> "+" | Minus -> "-" | Mult -> "*" in
        show lhs ^ " " ^ op_sym ^ " " ^ show rhs
    | Block (defs, result) ->
        let show_def = function
          | Texp.TypedVal.MonoDef (name, result) ->
              String.make ((indent + 1) * 4) ' '
              ^ name ^ " = "
              ^ show ~indent:(indent + 1) result
          | Texp.TypedVal.PolyDef (name, _, result) ->
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
    | Field (str, name, _) -> show ~indent str ^ "." ^ name
    | PatMatch (_, _, _) -> "match"
    | Lambda (_, arguments, body, _) ->
        "fun(" ^ String.concat ~sep:", " arguments ^ ")" ^ " = " ^ show body
    | Apply (fun_val, arg_vals, _) ->
        let arg_string = List.map arg_vals ~f:show in
        show fun_val ^ "(" ^ String.concat ~sep:", " arg_string ^ ")"

  let rec type_of = function
    | Var (_, t) -> t
    | Const (_, t) -> t
    | Oper (_, _, _, t) -> t
    | Block (_, result) -> type_of result
    | Field (_, _, t) -> t
    | PatMatch (_, _, t) -> t
    | Lambda (_, _, _, t) -> t
    | Apply (_, _, t) -> t
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

type name_check_error = UnknownName of string | UsedName of string

let rec capture_expr nctx = function
  | Texp.Var (name, t) ->
      let exists = NameContext.name_check nctx name in
      if exists then Result.Ok (Cexp.Var (name, t))
      else Result.Error (UnknownName name)
  | Texp.Const (value, t) -> Result.Ok (Cexp.Const (value, t))
  | Texp.Oper (op, lhs, rhs, t) ->
      let%bind clhs = capture_expr nctx lhs in
      let%bind crhs = capture_expr nctx rhs in
      Result.Ok (Cexp.Oper (op, clhs, crhs, t))
  | Texp.Apply (fun_val, arguments, t) ->
      let%bind cfun = capture_expr nctx fun_val in
      let%bind cargs =
        List.fold_right arguments ~init:(Result.Ok []) ~f:(fun x acc ->
            let%bind prev = acc in
            let%bind current = capture_expr nctx x in
            Result.Ok (current :: prev))
      in
      Result.Ok (Cexp.Apply (cfun, cargs, t))
  | Texp.Block (defs, result) ->
      let%bind rev_cdefs, block_ctx =
        List.fold_left defs
          ~init:(Result.Ok ([], nctx))
          ~f:(fun acc def ->
            let name, result, def =
              match def with
              | Texp.TypedVal.MonoDef (name, result) ->
                  (name, result, fun e -> Texp.TypedVal.MonoDef (name, e))
              | Texp.TypedVal.PolyDef (name, quants, result) ->
                  ( name,
                    result,
                    fun e -> Texp.TypedVal.PolyDef (name, quants, e) )
            in
            let%bind prev, block_context = acc in
            if not (NameContext.name_check block_context name) then
              let%bind current = capture_expr block_context result in
              Result.Ok
                (def current :: prev, NameContext.add_local block_context name)
            else Result.Error (UsedName name))
      in
      let%bind cresult = capture_expr block_ctx result in
      Result.Ok (Cexp.Block (List.rev rev_cdefs, cresult))
  | Texp.Lambda (arguments, result, t) ->
      let%bind lambda_ctx =
        List.fold_left arguments ~init:(Result.Ok nctx) ~f:(fun acc arg ->
            let%bind ctx = acc in
            if not (NameContext.name_check ctx arg) then
              Result.Ok (NameContext.add_local ctx arg)
            else Result.Error (UsedName arg))
      in
      let%bind cresult = capture_expr lambda_ctx result in
      let rec caught = function
        | Texp.Var (name, t) ->
            if NameContext.local_check nctx name then [ (name, t) ] else []
        | Texp.Oper (_, lhs, rhs, _) -> List.append (caught lhs) (caught rhs)
        | Texp.Apply (fun_val, arguments, _) ->
            let arguments_capture =
              List.concat (List.map arguments ~f:caught)
            in
            let fun_capture = caught fun_val in
            List.append arguments_capture fun_capture
        | Texp.Lambda (_, lambda_expression, _) -> caught lambda_expression
        | Texp.Block (defs, result) ->
            let def_capture =
              List.concat
                (List.map defs ~f:(fun symbol ->
                     caught (Texp.TypedVal.expression symbol)))
            in
            let result_capture = caught result in
            List.append def_capture result_capture
        | _ -> []
      in
      let lambda_catch = caught result in
      Result.Ok (Cexp.Lambda (lambda_catch, arguments, cresult, t))
  | Texp.PatMatch (obj, cases, t) ->
      let%bind cobj = capture_expr nctx obj in
      let%bind ccases =
        List.fold_right cases ~init:(Result.Ok []) ~f:(fun (d, e) acc ->
            let rec names c =
              match c with
              | Texp.Deconstructor.Var name -> [ name ]
              | Texp.Deconstructor.Constructor (_, deconstructors) ->
                  List.concat_map deconstructors ~f:(fun (d, _) -> names d)
            in
            let case_name_context = NameContext.add_locals nctx (names d) in
            let%bind res = capture_expr case_name_context e in
            let%map acc = acc in
            (d, res) :: acc)
      in
      Result.Ok (Cexp.PatMatch (cobj, ccases, t))
  | Texp.Field (str, name, t) ->
      let%bind cstr = capture_expr nctx str in
      let%bind cname =
        if NameContext.field_check nctx name then Result.Ok name
        else Result.Error (UnknownName name)
      in
      Result.Ok (Cexp.Field (cstr, cname, t))

let capture_handler nctx handler =
  let open Parsing.Ast.Handler in
  let%map _, rbody =
    List.fold_left handler.body
      ~init:(Result.Ok (nctx, []))
      ~f:(fun acc statement ->
        let%bind ctx_acc, s_acc = acc in
        match statement with
        | Spawn { name; actor } ->
            let%map cactor = capture_expr ctx_acc actor in
            ( NameContext.add_local ctx_acc name,
              Spawn { name; actor = cactor } :: s_acc )
        | Mutate e ->
            let%map ce = capture_expr ctx_acc e in
            (ctx_acc, Mutate ce :: s_acc)
        | Send { message; mail } ->
            let%bind cmessage = capture_expr ctx_acc message in
            let%map cmail = capture_expr ctx_acc mail in
            (ctx_acc, Send { message = cmessage; mail = cmail } :: s_acc)
        | Val typed_val ->
            let name, result, def =
              match typed_val with
              | Texp.TypedVal.MonoDef (name, result) ->
                  (name, result, fun e -> Texp.TypedVal.MonoDef (name, e))
              | Texp.TypedVal.PolyDef (name, quants, result) ->
                  ( name,
                    result,
                    fun e -> Texp.TypedVal.PolyDef (name, quants, e) )
            in
            let%map cresult = capture_expr ctx_acc result in
            (NameContext.add_local ctx_acc name, Val (def cresult) :: s_acc))
  in
  { handler with body = List.rev rbody }

let capture_program program =
  let open Texp.TProgram in
  let toplevel_names =
    let function_names =
      List.map program.functions ~f:(fun fun_symbol -> fun_symbol.name)
    in
    let constructor_names =
      List.bind program.types ~f:(fun type_symbol ->
          Ast.Datatype.constructors_names type_symbol)
    in
    let state_constructors_names =
      List.bind program.actors ~f:(fun actor ->
          List.map actor.states ~f:(fun state -> state.name))
    in
    List.append
      (List.append function_names constructor_names)
      state_constructors_names
  in
  let field_names =
    List.bind program.types ~f:(fun type_symbol ->
        List.bind
          (Ast.Datatype.constructors type_symbol)
          ~f:Ast.Datatype.field_names)
  in
  let%bind cfuncs =
    List.fold_left program.functions ~init:(Result.Ok []) ~f:(fun acc func ->
        let%bind acc_list = acc in
        let nctx = NameContext.init toplevel_names func.arguments field_names in
        let%map captured_body = capture_expr nctx func.expr in
        let fun_expression =
          Texp.TProgram.tfun func.name func.arguments func.fun_type
            captured_body
        in
        fun_expression :: acc_list)
  in
  let state_fields =
    let open List.Let_syntax in
    Map.of_alist_exn
      (module String)
      (let%bind actor = program.actors in
       let%map state = actor.states in
       (state.name, state.fields))
  in
  let%map chandlers =
    List.fold_left program.handlers ~init:(Result.Ok []) ~f:(fun acc handler ->
        let%bind acc_list = acc in
        let fields = Map.find_exn state_fields handler.state in
        let nctx =
          NameContext.init toplevel_names
            ("message" :: "me" :: List.map fields ~f:(fun (name, _) -> name))
            field_names
        in
        let%map captured_handler = capture_handler nctx handler in
        captured_handler :: acc_list)
  in
  {
    functions = cfuncs;
    types = program.types;
    actors = program.actors;
    handlers = chandlers;
  }
