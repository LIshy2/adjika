open Core
open Parsing

module Substitution = struct
  type t = Sub of (Tenv.Gen.type_id, Type.mono, Int.comparator_witness) Map.t

  let rec apply (Sub self) = function
    | Type.TypeVar x -> Option.value (Map.find self x) ~default:(Type.TypeVar x)
    | Type.Int -> Type.Int
    | Type.Arrow (args, result) ->
        let mapped_args = List.map args ~f:(apply (Sub self)) in
        let mapped_result = apply (Sub self) result in
        Type.Arrow (mapped_args, mapped_result)
    | custom -> custom

  let apply_poly self = function
    | Type.Mono mono -> Type.Mono (apply self mono)
    | Type.Quant (ids, mono) -> Type.Quant (ids, apply self mono)

  let apply_ctx self Tenv.{ local_env; fields_env; constructors } =
    Tenv.
      {
        local_env = Map.map local_env ~f:(fun t -> apply_poly self t);
        fields_env = Map.map fields_env ~f:(fun t -> apply_poly self t);
        constructors;
      }

  let compose (Sub lhs) (Sub rhs) =
    let updated_rhs = Map.map rhs ~f:(fun value -> apply (Sub lhs) value) in
    Sub
      (Map.merge_skewed lhs updated_rhs ~combine:(fun ~key v1 _ ->
           let _ = key in
           v1))

  let empty = Sub (Map.empty (module Int))
  let single tid pt = Sub (Map.singleton (module Int) tid pt)

  let show self =
    Map.iteri self ~f:(fun ~key ~data ->
        print_string (string_of_int key ^ ": " ^ Type.show_mono data ^ ", "));
    print_endline ""
end

let rec unify type_1 type_2 =
  match (type_1, type_2) with
  | Type.Int, Type.Int -> Substitution.empty
  | Type.TypeVar id, x -> Substitution.single id x
  | x, Type.TypeVar id -> Substitution.single id x
  | Arrow (type_args1, result_type1), Arrow (type_args2, result_type2) ->
      let args_substitution =
        List.fold_left (List.zip_exn type_args1 type_args2)
          ~init:Substitution.empty ~f:(fun acc (arg1, arg2) ->
            let u =
              unify (Substitution.apply acc arg1) (Substitution.apply acc arg2)
            in
            Substitution.compose acc u)
      in
      let result_substitution =
        unify
          (Substitution.apply args_substitution result_type1)
          (Substitution.apply args_substitution result_type2)
      in
      Substitution.compose args_substitution result_substitution
  | _ ->
      print_endline "Uninifiable types";
      print_endline (Type.show_mono type_1 ^ "|" ^ Type.show_mono type_2);
      exit (-1)

let rec w te = function
  | Capture.Cex.Var name ->
      let var_type = Tenv.find te name in
      let instantiated_type = Tenv.Gen.instantiate var_type in
      (Texp.Var (name, Mono instantiated_type), Substitution.empty)
  | Capture.Cex.Const value ->
      (Texp.Const (value, Mono Type.Int), Substitution.empty)
  | Capture.Cex.Lambda (captures, arguments, body) ->
      let type_vars =
        List.zip_exn arguments
          (List.init (List.length arguments) ~f:(fun _ -> Tenv.Gen.new_var ()))
      in
      let body_env =
        List.fold_left type_vars ~init:te ~f:(fun acc (name, id) ->
            Tenv.add acc name (Mono id))
      in
      let typed_body, lambda_sub = w body_env body in
      let arguments_types =
        List.map type_vars ~f:(fun (_, id) -> Substitution.apply lambda_sub id)
      in
      let lambda_type =
        Type.Arrow (arguments_types, Type.monotype (Texp.type_of typed_body))
      in
      let typed_captures =
        List.map captures ~f:(fun name -> (name, Tenv.find te name))
      in
      ( Texp.Lambda (typed_captures, arguments, typed_body, Mono lambda_type),
        lambda_sub )
  | Capture.Cex.Apply (fun_value, arguments) ->
      let typed_fun, fun_sub = w te fun_value in
      let typed_args, value_sub =
        let typed_args, args_sub =
          List.unzip
            (List.map arguments ~f:(w (Substitution.apply_ctx fun_sub te)))
        in
        let composed_sub =
          List.fold_left args_sub ~init:fun_sub ~f:(fun acc s ->
              Substitution.compose acc s)
        in
        (typed_args, composed_sub)
      in
      let new_type_id = Tenv.Gen.new_var () in
      let arg_types =
        List.map typed_args ~f:(fun texp -> Type.monotype (Texp.type_of texp))
      in
      let fun_type = Type.monotype (Texp.type_of typed_fun) in
      let u = unify (Type.Arrow (arg_types, new_type_id)) fun_type in
      let composed_sub = Substitution.compose u value_sub in
      let application_type = Substitution.apply u new_type_id in
      ( Texp.Apply
          ( typed_fun,
            typed_args,
            Mono application_type,
            Mono
              (Substitution.apply composed_sub
                 (Type.Arrow (arg_types, new_type_id))) ),
        composed_sub )
  | Capture.Cex.Oper (bop, lhs, rhs) ->
      let lt, l_sub = w te lhs in
      let rt, r_sub = w te rhs in
      let u_l = unify Type.Int (Type.monotype (Texp.type_of lt)) in
      let u_r = unify Type.Int (Type.monotype (Texp.type_of rt)) in
      let sub =
        Substitution.compose
          (Substitution.compose (Substitution.compose u_l u_r) l_sub)
          r_sub
      in
      (Texp.Oper (bop, lt, rt, Mono Type.Int), sub)
  | Capture.Cex.Field (str, field) ->
      let str_t, str_sub = w te str in
      let constructor = Tenv.Gen.instantiate (Tenv.find te field) in
      let new_type_id = Tenv.Gen.new_var () in
      let u =
        unify
          (Type.Arrow ([ Type.monotype (Texp.type_of str_t) ], new_type_id))
          constructor
      in
      let sub = Substitution.compose str_sub u in
      (Texp.Field (str_t, field, Mono (Substitution.apply sub new_type_id)), sub)
  | Capture.Cex.PatMatch (obj, cases) ->
      let tobj, obj_sub = w te obj in
      let case_sub, typed_cases =
        List.fold_left cases ~init:(Substitution.empty, [])
          ~f:(fun (sub_acc, typed_cases) (d, e) ->
            let rec extract c expected =
              match c with
              | Ast.Deconstructor.Var name ->
                  ( Substitution.empty,
                    Map.singleton (module String) name expected )
              | Ast.Deconstructor.Constructor (name, args) ->
                  let con = Tenv.constructor te name in
                  let deconstructors =
                    List.map2_exn args con.fields ~f:extract
                  in
                  let sub, env =
                    List.fold_left deconstructors
                      ~init:(Substitution.empty, Map.empty (module String))
                      ~f:(fun (sub_acc, env_acc) (sub, names) ->
                        let new_env = Map.merge_disjoint_exn names env_acc in
                        (Substitution.compose sub_acc sub, new_env))
                  in
                  (Substitution.compose sub (unify expected con.result), env)
            in
            let obj_type = Type.monotype (Texp.type_of tobj) in
            let sub, names = extract d obj_type in
            let case_env =
              Map.fold names ~init:te ~f:(fun ~key:name ~data:tpe acc ->
                  Tenv.add acc name (Type.Quant ([], tpe)))
            in
            let type_case, sub_case = w case_env e in
            ( Substitution.compose (Substitution.compose sub sub_case) sub_acc,
              type_case :: typed_cases ))
      in
      let match_sub = Substitution.compose obj_sub case_sub in
      let expected_type =
        Type.monotype (Texp.type_of (List.hd_exn typed_cases))
      in
      let result_sub =
        List.fold_left typed_cases ~init:Substitution.empty ~f:(fun sub_acc t ->
            let mt = Type.monotype (Texp.type_of t) in
            Substitution.compose sub_acc (unify expected_type mt))
      in
      let result_type = Substitution.apply result_sub expected_type in
      let tcases =
        List.map2_exn cases typed_cases ~f:(fun (d, _) c -> (d, c))
      in
      ( Texp.PatMatch (tobj, tcases, Mono result_type),
        Substitution.compose match_sub result_sub )
  | Capture.Cex.Block (defs, result) ->
      let rnew_defs, new_te, defs_sub =
        List.fold_left defs ~init:([], te, Substitution.empty)
          ~f:(fun (defs, type_env, sub) Ast.Symbol.{ name; result } ->
            let typed_expr, def_sub = w type_env result in
            let generialized = Texp.generealize type_env typed_expr in
            let update_env =
              Substitution.apply_ctx def_sub
                (Tenv.add type_env name (Texp.type_of generialized))
            in
            ( Ast.Symbol.{ name; result = generialized } :: defs,
              update_env,
              Substitution.compose sub def_sub ))
      in
      let typed_result, result_sub = w new_te result in
      let typed_defs = List.rev rnew_defs in
      ( Texp.Block (typed_defs, typed_result),
        Substitution.compose defs_sub result_sub )

let infer_expression te exp =
  let typed_ast, _ = w te exp in
  Texp.generealize te typed_ast

let infer_program program =
  let open Ast.Program in
  let toplevels, fields =
    let constructors =
      List.bind program.types ~f:(fun ts ->
          List.map ts.constructors ~f:(fun constructor ->
              let constructor_args =
                List.map constructor.fields ~f:(fun (_, tpe) ->
                    Type.from_decl tpe)
              in
              let constructor_result = Type.Custom ts.name in
              ( constructor.name,
                Type.Mono (Type.Arrow (constructor_args, constructor_result)) )))
    in
    let fields =
      List.bind program.types ~f:(fun ts ->
          match ts.constructors with
          | [ constructor ] ->
              let field_types =
                List.map (Ast.Symbol.field_types constructor) ~f:Type.from_decl
              in
              let get_functions =
                List.map2_exn field_types constructor.fields
                  ~f:(fun ft (name, _) ->
                    (name, Type.Mono (Type.Arrow ([ Type.Custom ts.name ], ft))))
              in
              get_functions
          | _ -> [])
    in
    (constructors, fields)
  in
  let typed_functions =
    List.map program.functions ~f:(fun Ast.Symbol.{ name; arguments; result } ->
        let cfun = Capture.Cex.Lambda ([], arguments, result) in
        let tenv = Tenv.of_list ~locals:toplevels ~fields in
        let typed_value = infer_expression tenv cfun in
        let expr =
          match typed_value with
          | Texp.Lambda (_, _, body, _) -> body
          | _ -> failwith "Bebebebe"
        in
        Texp.TypedProgram.
          { name; arguments; fun_type = Texp.type_of typed_value; expr })
  in
  let reformated_types =
    List.map program.types ~f:(fun t ->
        let constructors =
          List.map t.constructors ~f:(fun con ->
              let fields =
                List.map con.fields ~f:(fun (name, t) ->
                    (name, Type.from_decl t))
              in
              Ast.Toplevel.condecl con.name fields)
        in
        Ast.Symbol.{ name = t.name; constructors })
  in
  Texp.TypedProgram.{ functions = typed_functions; types = reformated_types }
