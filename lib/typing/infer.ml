open Core
open Parsing
open Core.List.Let_syntax

module Substitution = struct
  type t = Sub of (Tenv.Gen.type_id, Type.mono, Int.comparator_witness) Map.t

  let rec apply (Sub self) = function
    | Type.TypeVar x -> Option.value (Map.find self x) ~default:(Type.TypeVar x)
    | Type.Int -> Type.Int
    | Type.Arrow (args, result) ->
        let mapped_args = List.map args ~f:(apply (Sub self)) in
        let mapped_result = apply (Sub self) result in
        Type.Arrow (mapped_args, mapped_result)
    | Type.Operator (name, arguments) ->
        Type.Operator (name, List.map arguments ~f:(apply (Sub self)))
    | custom -> custom

  let apply_poly self = function
    | Type.Mono mono -> Type.Mono (apply self mono)
    | Type.Quant (ids, mono) -> Type.Quant (ids, apply self mono)

  let rec apply_ast sub = function
    | Texp.Var (name, t) -> Texp.Var (name, apply_poly sub t)
    | Texp.Const (value, t) -> Texp.Const (value, apply_poly sub t)
    | Texp.Oper (binop, lhs, rhs, t) ->
        Texp.Oper (binop, apply_ast sub lhs, apply_ast sub rhs, apply_poly sub t)
    | Texp.Lambda (arguments, body, t) ->
        Texp.Lambda (arguments, apply_ast sub body, apply_poly sub t)
    | Texp.Field (obj, name, t) ->
        Texp.Field (apply_ast sub obj, name, apply_poly sub t)
    | Texp.Block (vals, result) ->
        let apply_val Ast.Val.{ name; result } =
          Ast.Val.{ name; result = apply_ast sub result }
        in
        Texp.Block (List.map vals ~f:apply_val, apply_ast sub result)
    | Texp.PatMatch (obj, cases, t) ->
        let rec apply_dec = function
          | Texp.Deconstructor.Constructor (name, subs) ->
              Texp.Deconstructor.Constructor
                ( name,
                  List.map subs ~f:(fun (d, t) -> (apply_dec d, apply sub t)) )
          | var -> var
        in
        Texp.PatMatch
          ( apply_ast sub obj,
            List.map cases ~f:(fun (d, e) -> (apply_dec d, apply_ast sub e)),
            apply_poly sub t )
    | Texp.Apply (f, args, t) ->
        Texp.Apply
          (apply_ast sub f, List.map args ~f:(apply_ast sub), apply_poly sub t)

  let apply_ctx self tenv =
    tenv
    |> Tenv.apply_to_locals (apply_poly self)
    |> Tenv.apply_to_fields (apply_poly self)

  let empty = Sub (Map.empty (module Int))
  let single tid pt = Sub (Map.singleton (module Int) tid pt)

  exception ComposeFail of int

  let compose (Sub lhs) (Sub rhs) =
    let updated_rhs = Map.map rhs ~f:(fun value -> apply (Sub lhs) value) in
    let updated_lhs = Map.map lhs ~f:(fun value -> apply (Sub rhs) value) in
    Sub
      (Map.merge_skewed updated_lhs updated_rhs ~combine:(fun ~key v1 v2 ->
           if not (Type.compare_mono v1 v2 = 0) then raise (ComposeFail key)
           else v1))

  module Syntax = struct
    let ( +!+ ) = compose
  end
end

open Substitution.Syntax

exception Uninifiable of (Type.mono * Type.mono)

let rec unify type_1 type_2 =
  match (type_1, type_2) with
  | Type.Int, Type.Int -> Substitution.empty
  | Type.Named name1, Type.Named name2 ->
      if String.equal name1 name2 then Substitution.empty
      else raise (Uninifiable (type_1, type_2))
  | Type.TypeVar id, x -> Substitution.single id x
  | x, Type.TypeVar id -> Substitution.single id x
  | Arrow (type_args1, result_type1), Arrow (type_args2, result_type2) ->
      let args_substitution =
        List.fold_left (List.zip_exn type_args1 type_args2)
          ~init:Substitution.empty ~f:(fun acc (arg1, arg2) ->
            acc
            +!+ unify
                  (Substitution.apply acc arg1)
                  (Substitution.apply acc arg2))
      in
      let result_substitution =
        unify
          (Substitution.apply args_substitution result_type1)
          (Substitution.apply args_substitution result_type2)
      in
      args_substitution +!+ result_substitution
  | Type.Operator (name1, args1), Type.Operator (name2, args2) ->
      if String.equal name1 name2 then
        List.fold_left (List.zip_exn args1 args2) ~init:Substitution.empty
          ~f:(fun acc (arg1, arg2) ->
            acc
            +!+ unify
                  (Substitution.apply acc arg1)
                  (Substitution.apply acc arg2))
      else raise (Uninifiable (type_1, type_2))
  | Type.MailBox name1, Type.MailBox name2 ->
      if
        String.equal name1 name2 || String.equal name1 "?"
        || String.equal name2 "?"
      then Substitution.empty
      else raise (Uninifiable (type_1, type_2))
  | Type.Actor name1, Type.Actor name2 ->
      if
        String.equal name1 name2 || String.equal name1 "?"
        || String.equal name2 "?"
      then Substitution.empty
      else raise (Uninifiable (type_1, type_2))
  | _ -> raise (Uninifiable (type_1, type_2))

let rec w te = function
  | Ast.Expr.Var name ->
      let var_type = Tenv.find te name in
      let instantiated_type = Tenv.Gen.instantiate var_type in
      (Texp.Var (name, Mono instantiated_type), Substitution.empty)
  | Ast.Expr.Const value ->
      (Texp.Const (value, Mono Type.Int), Substitution.empty)
  | Ast.Expr.Lambda (arguments, body) ->
      let type_vars =
        List.zip_exn arguments
          (List.init (List.length arguments) ~f:(fun _ -> Tenv.Gen.new_var ()))
      in
      let body_env =
        List.fold_left type_vars ~init:te ~f:(fun acc (name, id) ->
            Tenv.add name (Mono id) acc)
      in
      let typed_body, lambda_sub = w body_env body in
      let arguments_types =
        List.map type_vars ~f:(fun (_, id) -> Substitution.apply lambda_sub id)
      in
      let lambda_type =
        Type.Arrow (arguments_types, Type.monotype (Texp.type_of typed_body))
      in
      (Texp.Lambda (arguments, typed_body, Mono lambda_type), lambda_sub)
  | Ast.Expr.Apply (fun_value, arguments) ->
      let typed_fun, fun_sub = w te fun_value in
      let typed_args, value_sub =
        let typed_args, args_sub =
          List.unzip
            (List.map arguments ~f:(w (Substitution.apply_ctx fun_sub te)))
        in
        let composed_sub =
          List.fold_left args_sub ~init:fun_sub ~f:(fun acc s -> acc +!+ s)
        in
        (typed_args, composed_sub)
      in
      let new_type_id = Tenv.Gen.new_var () in
      let arg_types =
        List.map typed_args ~f:(fun texp -> Type.monotype (Texp.type_of texp))
      in
      let fun_type = Type.monotype (Texp.type_of typed_fun) in
      let u = unify (Type.Arrow (arg_types, new_type_id)) fun_type in
      let composed_sub = u +!+ value_sub in
      let application_type = Substitution.apply u new_type_id in
      (Texp.Apply (typed_fun, typed_args, Mono application_type), composed_sub)
  | Ast.Expr.Oper (bop, lhs, rhs) ->
      let lt, l_sub = w te lhs in
      let rt, r_sub = w te rhs in
      let u_l = unify Type.Int (Type.monotype (Texp.type_of lt)) in
      let u_r = unify Type.Int (Type.monotype (Texp.type_of rt)) in
      let sub = u_l +!+ u_r +!+ l_sub +!+ r_sub in
      (Texp.Oper (bop, lt, rt, Mono Type.Int), sub)
  | Ast.Expr.Field (str, field) ->
      let str_t, str_sub = w te str in
      let constructor = Tenv.Gen.instantiate (Tenv.find te field) in
      let new_type_id = Tenv.Gen.new_var () in
      let u =
        unify
          (Type.Arrow ([ Type.monotype (Texp.type_of str_t) ], new_type_id))
          constructor
      in
      let sub = str_sub +!+ u in
      (Texp.Field (str_t, field, Mono (Substitution.apply sub new_type_id)), sub)
  | Ast.Expr.PatMatch (obj, cases) ->
      let tobj, obj_sub = w te obj in
      let case_sub, typed_cases =
        List.fold_left cases ~init:(obj_sub, [])
          ~f:(fun (sub_acc, typed_cases) (d, e) ->
            let rec extract c expected =
              match c with
              | Ast.Deconstructor.Var name ->
                  ( Substitution.empty,
                    Map.singleton (module String) name expected,
                    Texp.Deconstructor.Var name )
              | Ast.Deconstructor.Constructor (name, args) ->
                  let con =
                    Tenv.Constructor.instantiate (Tenv.constructor te name)
                  in
                  let fields =
                    List.map2_exn args (Tenv.Constructor.fields con) ~f:extract
                  in
                  let deconstructor =
                    Texp.Deconstructor.Constructor
                      ( name,
                        List.map2_exn fields (Tenv.Constructor.fields con)
                          ~f:(fun (_, _, d) t -> (d, t)) )
                  in
                  let sub, env =
                    List.fold_left fields
                      ~init:(Substitution.empty, Map.empty (module String))
                      ~f:(fun (sub_acc, env_acc) (sub, names, _) ->
                        let new_env = Map.merge_disjoint_exn names env_acc in
                        (sub_acc +!+ sub, new_env))
                  in
                  ( sub +!+ unify expected (Tenv.Constructor.instance con),
                    env,
                    deconstructor )
            in
            let obj_type = Type.monotype (Texp.type_of tobj) in
            let sub, names, deconstructor =
              extract d (Substitution.apply sub_acc obj_type)
            in
            let case_env =
              Map.fold names ~init:te ~f:(fun ~key:name ~data:tpe acc ->
                  Tenv.add name (Type.Mono tpe) acc)
            in
            let type_case, sub_case = w case_env e in
            ( sub_acc +!+ sub +!+ sub_case,
              (deconstructor, type_case) :: typed_cases ))
      in
      let match_sub = obj_sub +!+ case_sub in
      let expected_type =
        Type.monotype
          (Texp.type_of
             (let _, c = List.hd_exn typed_cases in
              c))
      in
      let result_sub =
        List.fold_left typed_cases ~init:match_sub ~f:(fun sub_acc (_, t) ->
            let mt = Type.monotype (Texp.type_of t) in
            sub_acc
            +!+ unify
                  (Substitution.apply sub_acc mt)
                  (Substitution.apply sub_acc expected_type))
      in
      let result_type = Substitution.apply result_sub expected_type in
      ( Texp.PatMatch (tobj, typed_cases, Mono result_type),
        match_sub +!+ result_sub )
  | Ast.Expr.Block (defs, result) ->
      let rnew_defs, new_te, defs_sub =
        List.fold_left defs ~init:([], te, Substitution.empty)
          ~f:(fun (defs, type_env, sub) Ast.Val.{ name; result } ->
            let typed_expr, def_sub = w type_env result in
            let generialized = Texp.generealize type_env typed_expr in
            let update_env =
              Substitution.apply_ctx def_sub
                (Tenv.add name (Texp.type_of generialized) type_env)
            in
            ( Ast.Val.{ name; result = generialized } :: defs,
              update_env,
              sub +!+ def_sub ))
      in
      let typed_result, result_sub = w new_te result in
      let typed_defs = List.rev rnew_defs in
      (Texp.Block (typed_defs, typed_result), defs_sub +!+ result_sub)

exception NotActor

let actor_to_mailbox = function
  | Type.Actor name -> Type.MailBox name
  | _ -> raise NotActor

let infer_handler te actor state_description handler =
  let open Ast.Handler in
  let message_type = Type.from_decl handler.message_type in

  let init_te =
    List.fold
      Ast.Actor.(state_description.fields)
      ~init:te
      ~f:(fun acc (name, tpe) -> Tenv.add name (Type.Mono tpe) acc)
    |> Tenv.add "message" (Type.Mono message_type)
    |> Tenv.add "me" (Type.Mono (Type.MailBox actor))
  in
  let _, _, rbody =
    List.fold_left handler.body ~init:(Substitution.empty, init_te, [])
      ~f:(fun (acc_sub, te, state_acc) s ->
        match s with
        | Spawn { name; actor = new_state } ->
            let typed_actor, sub_spawn = w te new_state in
            let state_type = Type.monotype (Texp.type_of typed_actor) in
            let sub = sub_spawn +!+ unify (Type.Actor "?") state_type in
            let actor_type =
              Type.Mono
                (actor_to_mailbox (Type.monotype (Texp.type_of typed_actor)))
            in
            let update_env =
              Substitution.apply_ctx sub (te |> Tenv.add name actor_type)
            in
            ( acc_sub +!+ sub,
              update_env,
              Spawn { name; actor = typed_actor } :: state_acc )
        | Val { name; result } ->
            let texpr, sub = w te result in
            let generialized = Texp.generealize te texpr in
            let expr_type = Texp.type_of generialized in
            let update_env =
              Substitution.apply_ctx sub (te |> Tenv.add name expr_type)
            in
            ( acc_sub +!+ sub,
              update_env,
              Val { name; result = generialized } :: state_acc )
        | Mutate new_state ->
            let tb, sub_mut = w te new_state in
            let generialized = Texp.generealize te tb in
            let state_type = Type.monotype (Texp.type_of tb) in
            let sub = sub_mut +!+ unify (Type.Actor actor) state_type in
            let update_env = Substitution.apply_ctx sub te in
            (acc_sub +!+ sub, update_env, Mutate generialized :: state_acc)
        | Send { message; mail } ->
            let typed_message, sub_message = w te message in
            let typed_mail, sub_mail =
              w (Substitution.apply_ctx sub_message te) mail
            in
            let mail_type = Type.monotype (Texp.type_of typed_mail) in
            let sub =
              sub_message +!+ sub_mail +!+ unify (Type.MailBox "?") mail_type
            in
            let update_env = Substitution.apply_ctx sub te in
            ( acc_sub +!+ sub,
              update_env,
              Send { message = typed_message; mail = typed_mail } :: state_acc
            ))
  in
  { message_type; state = handler.state; body = List.rev rbody }

module EnvExtractor = struct
  module PartialEnv = struct
    type context = {
      constructors : (string * Tenv.Constructor.poly) list;
      functions : (string * Type.poly) list;
      fields : (string * Type.poly) list;
    }

    let concat lhs rhs =
      {
        constructors = List.append lhs.constructors rhs.constructors;
        functions = List.append lhs.functions rhs.functions;
        fields = List.append lhs.fields rhs.fields;
      }

    module Syntax = struct
      let finalize { functions; fields; constructors } =
        Tenv.of_list ~locals:functions ~fields ~constructors

      let empty = { constructors = []; functions = []; fields = [] }

      let new_constructor name result fields =
        {
          constructors = [ (name, Tenv.Constructor.poly result fields) ];
          functions = [];
          fields = [];
        }

      let new_function name fun_type =
        { constructors = []; functions = [ (name, fun_type) ]; fields = [] }

      let new_field name fun_type =
        { constructors = []; functions = [ (name, fun_type) ]; fields = [] }

      let ( ++ ) lhs rhs = concat lhs rhs
    end
  end

  open PartialEnv.Syntax

  module TypesExtractor = struct
    open Ast.Datatype

    let extract_mono_constructor name constructors =
      List.fold_left constructors ~init:empty ~f:(fun acc constructor ->
          let args =
            List.map constructor.fields ~f:(fun (_, tpe) -> Type.from_decl tpe)
          in
          let result = Type.Named name in
          let function_type = Type.Arrow (args, result) in
          acc
          ++ new_constructor constructor.name (Type.Mono result) args
          ++ new_function constructor.name (Type.Mono function_type))

    let extract_poly_constructor name arguments constructors =
      let type_vars = List.map arguments ~f:(fun _ -> Tenv.Gen.new_var ()) in
      let var_ids =
        List.filter_map type_vars ~f:(fun t ->
            match t with Type.TypeVar id -> Some id | _ -> None)
      in
      let type_ctx =
        Map.of_alist_exn (module String) (List.zip_exn arguments type_vars)
      in
      List.fold_left constructors ~init:empty ~f:(fun acc constructor ->
          let args =
            List.map constructor.fields ~f:(fun (_, tpe) ->
                Type.from_decl_ctx type_ctx tpe)
          in
          let result = Type.Operator (name, type_vars) in
          let function_type = Type.Arrow (args, result) in
          acc
          ++ new_constructor constructor.name
               (Type.Quant (var_ids, result))
               args
          ++ new_function constructor.name (Type.Quant (var_ids, function_type)))

    let extract_constructors types =
      List.fold_left types ~init:empty ~f:(fun acc ts ->
          match ts with
          | Mono { name; constructors } ->
              acc ++ extract_mono_constructor name constructors
          | Operator { name; arguments; constructors } ->
              acc ++ extract_poly_constructor name arguments constructors)

    let extract_mono_fields name constructors =
      match constructors with
      | [ constructor ] ->
          let field_types =
            List.map (Ast.Datatype.field_types constructor) ~f:Type.from_decl
          in
          List.fold2_exn field_types constructor.fields ~init:empty
            ~f:(fun acc ft (field_name, _) ->
              acc
              ++ new_field field_name
                   (Type.Mono (Type.Arrow ([ Type.Named name ], ft))))
      | _ -> empty

    let extract_poly_fields name arguments constructors =
      match constructors with
      | [ constructor ] ->
          let type_vars =
            List.map arguments ~f:(fun _ -> Tenv.Gen.new_var ())
          in
          let var_ids =
            List.filter_map type_vars ~f:(fun t ->
                match t with Type.TypeVar id -> Some id | _ -> None)
          in
          let type_ctx =
            Map.of_alist_exn (module String) (List.zip_exn arguments type_vars)
          in
          let field_types =
            List.map
              (Ast.Datatype.field_types constructor)
              ~f:(Type.from_decl_ctx type_ctx)
          in
          let record_type = Type.Operator (name, type_vars) in
          List.fold2_exn field_types constructor.fields ~init:empty
            ~f:(fun acc ft (name, _) ->
              acc
              ++ new_field name
                   (Type.Quant (var_ids, Type.Arrow ([ record_type ], ft))))
      | _ -> empty

    let extract_fields types =
      List.fold types ~init:empty ~f:(fun acc ts ->
          match ts with
          | Mono { name; constructors } ->
              acc ++ extract_mono_fields name constructors
          | Operator { name; arguments; constructors } ->
              acc ++ extract_poly_fields name arguments constructors)
  end

  module ActorExtractor = struct
    let extract_constructors actors =
      let open Ast.Actor in
      List.fold actors ~init:empty ~f:(fun acc actor ->
          acc
          ++ List.fold actor.states ~init:empty ~f:(fun acc state ->
                 let args =
                   List.map state.fields ~f:(fun (_, tpe) -> Type.from_decl tpe)
                 in
                 let result = Type.Actor actor.name in
                 let function_type = Type.Arrow (args, result) in
                 acc
                 ++ new_constructor state.name (Type.Mono result) args
                 ++ new_function state.name (Type.Mono function_type)))
  end

  let extract program =
    let open Ast.Program in
    finalize
      (TypesExtractor.extract_constructors program.types
      ++ TypesExtractor.extract_fields program.types
      ++ ActorExtractor.extract_constructors program.actors)
end

let normalize_types types =
  let open Ast.Datatype in
  List.map types ~f:(fun t ->
      match t with
      | Mono { name; constructors } ->
          let constructors =
            List.map constructors ~f:(fun con ->
                let fields =
                  List.map con.fields ~f:(fun (name, t) ->
                      (name, Type.from_decl t))
                in
                Ast.Datatype.constructor con.name fields)
          in
          Ast.Datatype.Mono { name; constructors }
      | Operator { name; arguments; constructors } ->
          let type_vars =
            List.map arguments ~f:(fun _ -> Tenv.Gen.new_var ())
          in
          let int_args =
            List.filter_map type_vars ~f:(fun k ->
                match k with Type.TypeVar id -> Some id | _ -> None)
          in
          let type_ctx =
            Map.of_alist_exn (module String) (List.zip_exn arguments type_vars)
          in
          let constructors =
            List.map constructors ~f:(fun con ->
                let fields =
                  List.map con.fields ~f:(fun (name, t) ->
                      (name, Type.from_decl_ctx type_ctx t))
                in
                Ast.Datatype.constructor con.name fields)
          in
          Ast.Datatype.Operator { name; arguments = int_args; constructors })

let normalize_actors actors =
  let open Ast.Actor in
  List.map actors ~f:(fun actor ->
      decl actor.name
        (List.map actor.states ~f:(fun s ->
             let typed_fields =
               List.map s.fields ~f:(fun (name, t) -> (name, Type.from_decl t))
             in
             state s.name typed_fields)))

let state_to_actor_map actors =
  let open Ast.Actor in
  let map =
    Map.of_alist_exn
      (module String)
      (let%bind actor = actors in
       let%map state = actor.states in
       (state.name, (actor.name, state)))
  in
  Map.find_exn map

let infer_program program =
  let open Ast.Program in
  let tenv = EnvExtractor.extract program in
  let _, function_env, typed_functions =
    List.fold_left program.functions ~init:(Substitution.empty, tenv, [])
      ~f:(fun
          (sub, env, typed_result) Ast.Function.{ name; arguments; result } ->
        let arguments_vars =
          List.map arguments ~f:(fun name -> (name, Tenv.Gen.new_var ()))
        in
        let result_type = Tenv.Gen.new_var () in
        let type_arguments = List.map arguments_vars ~f:(fun (_, t) -> t) in
        let function_type =
          Type.Mono (Type.Arrow (type_arguments, result_type))
        in
        let body_env =
          List.fold arguments_vars ~init:env ~f:(fun acc (name, t) ->
              Tenv.add name (Type.Mono t) acc)
          |> Tenv.add name function_type
        in
        let infered_body, body_sub = w body_env result in
        let body_type = Texp.type_of infered_body in
        let sub_result = unify (Type.monotype body_type) result_type in
        let final_sub = sub_result +!+ sub +!+ body_sub in
        let finalize_body = Substitution.apply_ast final_sub infered_body in
        let fnty =
          Tenv.generealize Tenv.empty
            (Substitution.apply final_sub (Type.monotype function_type))
        in
        let global_env = Tenv.add name fnty env in
        ( final_sub,
          global_env,
          Texp.TProgram.tfun name arguments
            (Substitution.apply_poly final_sub fnty)
            finalize_body
          :: typed_result ))
  in
  let normal_types = normalize_types program.types in
  let normal_actors = normalize_actors program.actors in
  let state_to_actor = state_to_actor_map normal_actors in
  let typed_handlers =
    List.map program.handlers ~f:(fun h ->
        let actor, state = state_to_actor h.state in
        infer_handler function_env actor state h)
  in
  Texp.TProgram.
    {
      functions = typed_functions;
      types = normal_types;
      actors = normal_actors;
      handlers = typed_handlers;
    }
