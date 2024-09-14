open Core
open Parsing
open Core.List.Let_syntax

let rec w te expr =
  let open Substitution.Let_syntax in
  match expr with
  | Ast.Expr.Var name ->
      let%map var_type = Substitution.apply_poly (Tenv.find te name) in
      let instantiated_type = Tenv.Gen.instantiate var_type in
      Texp.Var (name, Mono instantiated_type)
  | Ast.Expr.Const value -> return (Texp.Const (value, Mono Type.Int))
  | Ast.Expr.Lambda (arguments, body) ->
      let arguments_types =
        List.init (List.length arguments) ~f:(fun _ -> Tenv.Gen.new_var ())
      in
      let named_types = List.zip_exn arguments arguments_types in
      let body_env =
        List.fold_left named_types ~init:te ~f:(fun acc (name, id) ->
            Tenv.add name (Mono id) acc)
      in
      let%bind typed_body = w body_env body in
      let%map lambda_type =
        Substitution.apply
          (Type.Arrow (arguments_types, Type.monotype (Texp.type_of typed_body)))
      in
      Texp.Lambda (arguments, typed_body, Mono lambda_type)
  | Ast.Expr.Apply (func, arguments) ->
      let%bind typed_fun = w te func in
      let%bind typed_args = Substitution.compose_list arguments ~f:(w te) in
      let arg_types =
        List.map typed_args ~f:(fun texp -> Type.monotype (Texp.type_of texp))
      in
      let%bind fun_type =
        Substitution.apply (Type.monotype (Texp.type_of typed_fun))
      in
      let return_type = Tenv.Gen.new_var () in
      let%bind _ =
        Substitution.unify (Type.Arrow (arg_types, return_type)) fun_type
      in
      let%map application_type = Substitution.apply return_type in
      Texp.Apply (typed_fun, typed_args, Mono application_type)
  | Ast.Expr.Oper (bop, lhs, rhs) ->
      let%bind lt = w te lhs in
      let%bind rt = w te rhs in
      let%bind _ =
        Substitution.unify Type.Int (Type.monotype (Texp.type_of lt))
      in
      let%map _ =
        Substitution.unify Type.Int (Type.monotype (Texp.type_of rt))
      in
      Texp.Oper (bop, lt, rt, Mono Type.Int)
  | Ast.Expr.Field (str, field) ->
      let%bind str = w te str in
      let accessor = Tenv.Gen.instantiate (Tenv.find_field te field) in
      let new_type_id = Tenv.Gen.new_var () in
      let%bind _ =
        Substitution.unify
          (Type.Arrow ([ Type.monotype (Texp.type_of str) ], new_type_id))
          accessor
      in
      let%map field_type = Substitution.apply new_type_id in
      Texp.Field (str, field, Mono field_type)
  | Ast.Expr.PatMatch (obj, cases) ->
      let%bind tobj = w te obj in
      let%bind typed_cases =
        List.fold_left cases ~init:(return []) ~f:(fun acc (d, e) ->
            let%bind typed_cases = acc in
            let rec extract c expected =
              match c with
              | Ast.Deconstructor.Var name ->
                  return
                    ( Map.singleton (module String) name expected,
                      Texp.Deconstructor.Var name )
              | Ast.Deconstructor.Constructor (name, args) ->
                  let con =
                    Tenv.Constructor.instantiate (Tenv.constructor te name)
                  in
                  let named_fields =
                    List.zip_exn args (Tenv.Constructor.fields con)
                  in
                  let%bind fields =
                    Substitution.compose_list named_fields ~f:(fun (c, t) ->
                        extract c t)
                  in
                  let names, sub_constructors = List.unzip fields in
                  let deconstructor =
                    Texp.Deconstructor.Constructor
                      ( name,
                        List.zip_exn sub_constructors
                          (Tenv.Constructor.fields con) )
                  in
                  let env =
                    List.fold_left names
                      ~init:(Map.empty (module String))
                      ~f:(fun env_acc env -> Map.merge_disjoint_exn env env_acc)
                  in
                  let%map _ =
                    Substitution.unify expected (Tenv.Constructor.instance con)
                  in
                  (env, deconstructor)
            in
            let%bind obj_type =
              Substitution.apply (Type.monotype (Texp.type_of tobj))
            in
            let%bind names, deconstructor = extract d obj_type in
            let case_env =
              Map.fold names ~init:te ~f:(fun ~key:name ~data:tpe acc ->
                  Tenv.add name (Type.Mono tpe) acc)
            in
            let%map type_case = w case_env e in
            (deconstructor, type_case) :: typed_cases)
      in
      let%bind expected_type =
        Substitution.apply
          (Type.monotype
             (Texp.type_of
                (let _, c = List.hd_exn typed_cases in
                 c)))
      in
      let%bind _ =
        List.fold_left typed_cases ~init:(return ()) ~f:(fun sub_acc (_, t) ->
            let%bind _ = sub_acc in
            let%bind branch_type =
              Substitution.apply (Type.monotype (Texp.type_of t))
            in
            let%bind expected = Substitution.apply expected_type in
            Substitution.unify branch_type expected)
      in
      let%map result_type = Substitution.apply expected_type in
      Texp.PatMatch (tobj, typed_cases, Mono result_type)
  | Ast.Expr.Block (defs, result) ->
      let%bind rnew_defs, new_te =
        List.fold_left defs
          ~init:(return ([], te))
          ~f:(fun acc Ast.Val.{ name; result } ->
            let%bind defs, type_env = acc in
            let%map typed_expr = w type_env result in
            let generialized = Texp.generealize type_env typed_expr in
            let update_env =
              Tenv.add name (Texp.type_of generialized) type_env
            in
            (Ast.Val.{ name; result = generialized } :: defs, update_env))
      in
      let%map typed_result = w new_te result in
      let typed_defs = List.rev rnew_defs in
      Texp.Block (typed_defs, typed_result)

exception NotActor

let actor_to_mailbox = function
  | Type.Actor name -> Type.MailBox name
  | _ -> raise NotActor

let infer_handler te actor state_description handler =
  let open Ast.Handler in
  let open Substitution.Let_syntax in
  let message_type = Type.from_decl handler.message_type in
  let init_te =
    List.fold
      Ast.Actor.(state_description.fields)
      ~init:te
      ~f:(fun acc (name, tpe) -> Tenv.add name (Type.Mono tpe) acc)
    |> Tenv.add "message" (Type.Mono message_type)
    |> Tenv.add "me" (Type.Mono (Type.MailBox actor))
  in
  let%map _, rbody =
    List.fold_left handler.body
      ~init:(return (init_te, []))
      ~f:(fun acc s ->
        let%bind te, state_acc = acc in
        match s with
        | Spawn { name; actor = new_state } ->
            let%bind typed_actor = w te new_state in
            let%bind state_type =
              Substitution.apply (Type.monotype (Texp.type_of typed_actor))
            in
            let%map _ = Substitution.unify (Type.Actor "?") state_type in
            let actor_type =
              Type.Mono
                (actor_to_mailbox (Type.monotype (Texp.type_of typed_actor)))
            in
            let update_env = te |> Tenv.add name actor_type in
            (update_env, Spawn { name; actor = typed_actor } :: state_acc)
        | Val { name; result } ->
            let%map texpr = w te result in
            let generialized = Texp.generealize te texpr in
            let expr_type = Texp.type_of generialized in
            let update_env = te |> Tenv.add name expr_type in
            (update_env, Val { name; result = generialized } :: state_acc)
        | Mutate new_state ->
            let%bind tb = w te new_state in
            let generialized = Texp.generealize te tb in
            let state_type = Type.monotype (Texp.type_of tb) in
            let%map _ = Substitution.unify (Type.Actor actor) state_type in
            (te, Mutate generialized :: state_acc)
        | Send { message; mail } ->
            let%bind typed_message = w te message in
            let%bind typed_mail = w te mail in
            let%bind mail_type =
              Substitution.apply (Type.monotype (Texp.type_of typed_mail))
            in
            let%map _ = Substitution.unify mail_type (Type.MailBox "?") in
            ( te,
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
        { constructors = []; functions = []; fields = [ (name, fun_type) ] }

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
  let open Substitution.Let_syntax in
  let tenv = EnvExtractor.extract program in
  let function_env, typed_functions =
    Substitution.run_substitution
      (List.fold_left program.functions
         ~init:(return (tenv, []))
         ~f:(fun acc Ast.Function.{ name; arguments; result } ->
           let%bind env, typed_result = acc in
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
           let%bind infered_body = w body_env result in
           let body_type = Texp.type_of infered_body in
           let%bind _ =
             Substitution.unify (Type.monotype body_type) result_type
           in
           let%bind finalize_body = Substitution.apply_ast infered_body in
           let%map final_type =
             Substitution.apply (Type.monotype function_type)
           in
           let fnty = Tenv.generealize Tenv.empty final_type in
           let global_env = Tenv.add name fnty env in
           let new_tfun =
             Texp.TProgram.tfun name arguments fnty finalize_body
           in
           (global_env, new_tfun :: typed_result)))
  in
  let normal_types = normalize_types program.types in
  let normal_actors = normalize_actors program.actors in
  let state_to_actor = state_to_actor_map normal_actors in
  let typed_handlers =
    List.map program.handlers ~f:(fun h ->
        let actor, state = state_to_actor h.state in
        Substitution.run_substitution (infer_handler function_env actor state h))
  in
  Texp.TProgram.
    {
      functions = typed_functions;
      types = normal_types;
      actors = normal_actors;
      handlers = typed_handlers;
    }
