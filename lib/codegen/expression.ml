open Parsing.Ast
open Core
open Llvm
open Typing
open Analys.Capture
open Context

module VarCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)
  module TypeMap = PolyCtx (LL)

  let find_var_value type_ctx bind realization_type =
    let representation = TypeMap.type_to_bind type_ctx realization_type in
    Bind.realization bind representation

  let compile_var CC.{ bind_ctx; builder; type_ctx; runtime; _ } name tpe =
    let result =
      match BindingCtx.find bind_ctx name with
      | BindingCtx.Local local -> find_var_value type_ctx local tpe
      | BindingCtx.Top top ->
          let build_capture var_value =
            let fun_str_tpe = struct_type LL.ctx [| pointer_type LL.ctx |] in
            let instance = build_malloc fun_str_tpe (name ^ ".inst") builder in
            let _ = build_store var_value instance builder in
            instance
          in
          build_capture (find_var_value type_ctx top tpe)
    in
    match tpe with
    | Type.Int -> result
    | _ -> Runtime.build_dup runtime result name builder
end

module ConstCompiler (LL : LowLevelCtx) = struct
  let compile_const value = const_int (i64_type LL.ctx) value
end

module OperatorCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)

  let compile_operator compiler CC.{ builder; _ } bop lhs rhs =
    let command, name =
      match bop with
      | BinOp.Plus -> (build_add, "plus")
      | Minus -> (build_sub, "minus")
      | Mult -> (build_mul, "mult")
    in
    let lvalue = compiler lhs in
    let rvalue = compiler rhs in
    command lvalue rvalue name builder
end

module ApplyCompiler (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module CC = CompilerCtx (LL)

  let boxed_call builder fun_tpy fun_val arg_vals =
    let function_struct = struct_type LL.ctx [| pointer_type LL.ctx |] in
    let function_ptr =
      build_struct_gep function_struct fun_val 0 "function_ptr" builder
    in
    let function_val =
      build_load (pointer_type LL.ctx) function_ptr "function_val" builder
    in
    build_call fun_tpy function_val
      (Array.of_list (function_val :: arg_vals))
      "apply" builder

  let compile_apply compiler CC.{ builder; type_ctx; _ } fun_value arg_values =
    let spec_tpe = Cexp.type_of fun_value in
    let fun_value = compiler fun_value in
    let arguments_values = List.map arg_values ~f:(fun arg -> compiler arg) in
    let function_type = TypeMap.lltype_ref type_ctx spec_tpe in
    let call = boxed_call builder function_type fun_value arguments_values in
    call
end

module LambdaCompiler (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module CC = CompilerCtx (LL)

  module Capture = struct
    type t = {
      struct_tpe : lltype;
      value : llvalue;
      capture_values : (string * Bind.t) list;
    }

    let build lambda_fun capture_values bd =
      let capture_context_type =
        let flat_capture_types =
          List.bind capture_values ~f:(fun (_, x) ->
              match x with
              | Bind.Single s -> [ type_of s ]
              | Bind.Versions v -> List.map v ~f:(fun (_, v) -> type_of v))
        in
        let append_function_to_end =
          pointer_type LL.ctx :: flat_capture_types
        in
        struct_type LL.ctx (Array.of_list append_function_to_end)
      in
      let lambda_instance = build_malloc capture_context_type "lcapture" bd in
      let _ =
        List.fold_left capture_values ~init:1 ~f:(fun ind (name, bind) ->
            match bind with
            | Bind.Single s ->
                let field_ptr =
                  build_struct_gep capture_context_type lambda_instance ind
                    (name ^ ".capture.ptr") bd
                in
                let _ = build_store s field_ptr bd in
                ind + 1
            | Bind.Versions v ->
                let _ =
                  List.mapi v ~f:(fun ind (_, v) ->
                      let field_ptr =
                        build_struct_gep capture_context_type lambda_instance
                          ind (name ^ ".capture.ptr") bd
                      in
                      build_store v field_ptr bd)
                in
                ind + List.length v)
      in
      let fun_ptr =
        build_struct_gep capture_context_type lambda_instance 0
          "lcapture_fun_ptr" bd
      in
      let _ = build_store lambda_fun fun_ptr bd in
      {
        struct_tpe = capture_context_type;
        value = lambda_instance;
        capture_values;
      }

    let extract cap bd =
      let rec loop ind = function
        | (name, Bind.Single s) :: tail ->
            let field_ptr =
              build_struct_gep cap.struct_tpe cap.value ind (name ^ ".ptr") bd
            in
            let field_value = build_load (type_of s) field_ptr name bd in
            (name, Bind.Single field_value) :: loop (ind + 1) tail
        | (name, Bind.Versions v) :: tail ->
            ( name,
              Bind.Versions
                (List.mapi v ~f:(fun add_ind (t, v) ->
                     let field_ptr =
                       build_struct_gep cap.struct_tpe cap.value (ind + add_ind)
                         (name ^ "_ptr") bd
                     in
                     let field_value =
                       build_load (type_of v) field_ptr name bd
                     in
                     (t, field_value))) )
            :: loop (ind + List.length v) tail
        | [] -> []
      in
      loop 1 cap.capture_values
  end

  let build_lambda_body compiler ctx cap arg_names arg_values body =
    let arguments_binds =
      List.map2_exn arg_names arg_values ~f:(fun name value ->
          (name, Bind.Single value))
    in
    let capture_binds = Capture.extract cap CC.(ctx.builder) in
    let initial_bindings = List.append arguments_binds capture_binds in
    let lambda_ctx =
      CC.
        {
          ctx with
          bind_ctx = BindingCtx.replace_locals ctx.bind_ctx initial_bindings;
        }
    in
    let lambda_result = compiler lambda_ctx body in
    let _ = build_ret lambda_result CC.(ctx.builder) in
    ()

  let compile_lambda compiler ctx captures arguments body lambda_type =
    let lambda_function_tpy =
      TypeMap.lltype_ref CC.(ctx.type_ctx) lambda_type
    in
    let lambda_fun = define_function "lambda" lambda_function_tpy LL.md in
    let context_value, arguments_values =
      let llvm_args = List.of_array (params lambda_fun) in
      let context_val = List.hd_exn llvm_args in
      let arguments_val = List.tl_exn llvm_args in
      (context_val, arguments_val)
    in
    let _ =
      List.iter2_exn arguments arguments_values ~f:(fun name value ->
          set_value_name name value)
    in
    let capture_bindings =
      List.map captures ~f:(fun (name, _) ->
          (name, BindingCtx.local CC.(ctx.bind_ctx) name))
    in
    let cap = Capture.build lambda_fun capture_bindings ctx.builder in
    let lambda_bd = builder_at_end LL.ctx (entry_block lambda_fun) in
    build_lambda_body compiler
      CC.{ ctx with builder = lambda_bd }
      Capture.{ cap with value = context_value }
      arguments arguments_values body;
    cap.value
end

module BlockCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)
  module TypeMap = PolyCtx (LL)

  let compile_block compiler ctx defs result =
    let compile_def dctx = function
      | Texp.TypedVal.MonoDef (name, expr) ->
          let result_value = compiler dctx expr in
          let bind_type =
            TypeMap.type_to_bind CC.(dctx.type_ctx) (Cexp.type_of expr)
          in
          let bind = Bind.Single result_value in
          let resource = Scope.Resources.single result_value bind_type in
          (name, bind, resource)
      | Texp.TypedVal.PolyDef (name, quants, expr) ->
          let variants = TypeMap.variants CC.(dctx.type_ctx) quants in
          let compiled_variants =
            List.map variants ~f:(fun c ->
                let new_context = CC.{ dctx with type_ctx = c } in
                let value = compiler new_context expr in
                (TypeMap.type_to_bind c (Cexp.type_of expr), value))
          in
          let resources =
            Scope.Resources.of_list
              (List.map compiled_variants ~f:(fun (t, v) ->
                   Scope.Resources.obj v t))
          in
          (name, Bind.Versions compiled_variants, resources)
    in
    let new_context, resources =
      List.fold_left defs
        ~init:(ctx, Scope.Resources.of_list [])
        ~f:(fun (acc_ctx, acc_res) def ->
          let name, bind, resources = compile_def acc_ctx def in
          Bind.set_name bind name;
          let new_ctx =
            CC.{ ctx with bind_ctx = BindingCtx.add name bind acc_ctx.bind_ctx }
          in
          (new_ctx, Scope.Resources.concat resources acc_res))
    in
    let block_result = compiler new_context result in
    let _ = Scope.desctruct resources ctx.runtime ctx.builder in
    block_result
end

module FieldCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)
  module TypeMap = PolyCtx (LL)

  let compile_field compiler CC.{ bind_ctx; builder; type_ctx; runtime; _ } str
      name tpe =
    let str_val = compiler str in
    let accessor_bind = BindingCtx.accessor bind_ctx name in
    let accessor_type =
      TypeMap.type_to_bind type_ctx (Type.Arrow ([ Cexp.type_of str ], tpe))
    in
    let accessor = Bind.realization accessor_bind accessor_type in
    let fnty =
      function_type (TypeMap.lltype_ref type_ctx tpe) [| pointer_type LL.ctx |]
    in
    let call = build_call fnty accessor [| str_val |] name builder in
    match tpe with
    | Type.Int -> call
    | _ -> Runtime.build_dup runtime call name builder
end

module MatchCompiler (LL : LowLevelCtx) = struct
  open CompilerCtx (LL)
  module TagMap = TagsCtx (LL)
  module PolyMap = PolyCtx (LL)

  let deconstructor_layout tag_ctx poly_ctx name sub_decs =
    let fields =
      List.map sub_decs ~f:(fun (_, t) -> PolyMap.lltype_elem poly_ctx t)
    in
    let tag = TagMap.tag_type tag_ctx name in
    struct_type LL.ctx (Array.of_list (tag :: fields))

  let rec case_check tag_ctx poly_ctx dec ptr builder =
    match dec with
    | Texp.Deconstructor.Var _ -> const_int (i1_type LL.ctx) 1
    | Texp.Deconstructor.Constructor (name, sub_decs) ->
        let tag_checker = TagMap.tag_checker tag_ctx name in
        let constructor_check =
          build_call TagMap.checker_tpe tag_checker [| ptr |] (name ^ ".check")
            builder
        in
        let layout = deconstructor_layout tag_ctx poly_ctx name sub_decs in
        let field_types = struct_element_types layout in
        let sub_checks =
          List.mapi sub_decs ~f:(fun ind (dec, _) ->
              let field_type = field_types.(ind + 1) in
              let field =
                let ptr =
                  build_struct_gep layout ptr (ind + 1) "ptr_field" builder
                in
                build_load field_type ptr "value" builder
              in
              case_check tag_ctx poly_ctx dec field builder)
        in
        List.fold_left sub_checks ~init:constructor_check ~f:(fun acc x ->
            build_mul acc x "con_and" builder)

  let rec extract_values tag_ctx poly_ctx dec ptr builder =
    match dec with
    | Texp.Deconstructor.Var name ->
        Map.singleton (module String) name (Bind.Single ptr)
    | Texp.Deconstructor.Constructor (name, sub_decs) ->
        let layout = deconstructor_layout tag_ctx poly_ctx name sub_decs in
        let field_types = struct_element_types layout in
        let sub_values =
          List.mapi sub_decs ~f:(fun ind (dec, _) ->
              let field_type = field_types.(ind + 1) in
              let field =
                let ptr =
                  build_struct_gep layout ptr (ind + 1) "ptr_field" builder
                in
                build_load field_type ptr "value" builder
              in
              extract_values tag_ctx poly_ctx dec field builder)
        in
        List.fold_left sub_values
          ~init:(Map.empty (module String))
          ~f:(fun acc v -> Map.merge_disjoint_exn acc v)

  type match_blocks = {
    checkers : llbasicblock list;
    branches : llbasicblock list;
    unmatched : llbasicblock;
    after : llbasicblock;
  }

  let generate_blocks cases par_fun =
    let checkers =
      List.map cases ~f:(fun _ -> append_block LL.ctx ".checker" par_fun)
    in
    let branches =
      List.map cases ~f:(fun _ -> append_block LL.ctx ".branch" par_fun)
    in
    let unmatched_block = append_block LL.ctx ".unmatched" par_fun in
    let after_match_block = append_block LL.ctx ".after_match" par_fun in
    {
      checkers;
      branches;
      unmatched = unmatched_block;
      after = after_match_block;
    }

  let build_unmatched_block unmatched =
    let _ = build_unreachable (builder_at_end LL.ctx unmatched) in
    ()

  let compile_match compiler ctx obj cases =
    let obj = compiler ctx obj in
    let control_block = insertion_block ctx.builder in
    let par_fun = block_parent control_block in
    let flow = generate_blocks cases par_fun in
    build_unmatched_block flow.unmatched;
    let _ = build_br (List.hd_exn flow.branches) ctx.builder in
    let _ =
      let rec loop cases checkers branches =
        match (cases, checkers, branches) with
        | ( (this_case, _) :: other_cases,
            this_checker :: else_checker :: other_checkers,
            this_branch :: other_branches ) ->
            let builder = builder_at_end LL.ctx this_checker in
            let condition =
              case_check ctx.tag_ctx ctx.type_ctx this_case obj builder
            in
            let _ = build_cond_br condition this_branch else_checker builder in
            loop other_cases (else_checker :: other_checkers) other_branches
        | [ (last_case, _) ], [ last_checker ], [ last_branch ] ->
            let builder = builder_at_end LL.ctx last_checker in
            let condition =
              case_check ctx.tag_ctx ctx.type_ctx last_case obj builder
            in
            let _ =
              build_cond_br condition last_branch flow.unmatched builder
            in
            ()
        | _ -> failwith "can't zip3 cases, checkers, branches"
      in
      loop cases flow.checkers flow.branches
    in
    let branch_results =
      List.map2_exn cases flow.branches ~f:(fun (deconstr, expr) branch ->
          let builder = builder_at_end LL.ctx branch in
          let extract_names =
            extract_values ctx.tag_ctx ctx.type_ctx deconstr obj builder
          in
          let result =
            compiler
              {
                ctx with
                builder;
                bind_ctx = BindingCtx.append_locals ctx.bind_ctx extract_names;
              }
              expr
          in
          let _ = build_br flow.after builder in
          (result, branch))
    in
    ctx.builder <- builder_at_end LL.ctx flow.after;
    let phi_node = build_phi branch_results ".match_result" ctx.builder in
    phi_node
end

module ExpressionCompiler (LL : LowLevelCtx) = struct
  module VarC = VarCompiler (LL)
  module ConstC = ConstCompiler (LL)
  module OperC = OperatorCompiler (LL)
  module ApplyC = ApplyCompiler (LL)
  module LambdaC = LambdaCompiler (LL)
  module BlockC = BlockCompiler (LL)
  module FieldC = FieldCompiler (LL)
  module PatMatchC = MatchCompiler (LL)

  let rec compile_expression ctx expr =
    match expr with
    | Cexp.Var (name, tpe) -> VarC.compile_var ctx name tpe
    | Cexp.Const (value, _) -> ConstC.compile_const value
    | Cexp.Oper (bop, lhs, rhs, _) ->
        OperC.compile_operator (compile_expression ctx) ctx bop lhs rhs
    | Cexp.Apply (fu, arguments, _) ->
        ApplyC.compile_apply (compile_expression ctx) ctx fu arguments
    | Cexp.Lambda (captures, arguments, result, lambda_type) ->
        LambdaC.compile_lambda compile_expression ctx captures arguments result
          lambda_type
    | Cexp.PatMatch (obj, cases, _) ->
        PatMatchC.compile_match compile_expression ctx obj cases
    | Cexp.Block (defs, result) ->
        BlockC.compile_block compile_expression ctx defs result
    | Cexp.Field (str, name, tpe) ->
        FieldC.compile_field (compile_expression ctx) ctx str name tpe
end
