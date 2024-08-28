open Parsing.Ast
open Core
open Llvm
open Typing

module Bind = struct
  type t = Single of llvalue | Versions of (lltype * llvalue) list

  exception PolyBindError

  let single_exn = function Single v -> v | _ -> raise PolyBindError
end

module type LowLevelCtx = sig
  val ctx : llcontext
  val md : llmodule
end

module PolyCtx (LL : LowLevelCtx) = struct
  type t = TypeMap of (int, lltype, Int.comparator_witness) Map.t

  let empty = TypeMap (Map.empty (module Int))

  let variant (TypeMap map) id =
    [
      TypeMap (Map.add_exn map ~key:id ~data:(i64_type LL.ctx));
      TypeMap (Map.add_exn map ~key:id ~data:(pointer_type LL.ctx));
    ]

  let rec variants tm = function
    | id :: other ->
        let prev_ctx = variants tm other in
        List.bind prev_ctx ~f:(fun ctx -> variant ctx id)
    | [] -> [ tm ]

  let lltype_no_ctx = function
    | Type.Int -> i64_type LL.ctx
    | Type.Arrow (args, result) ->
        let box_type = function
          | Type.Int -> i64_type LL.ctx
          | Type.TypeVar _ -> failwith "Unknown type var"
          | _ -> pointer_type LL.ctx
        in
        let args_type = pointer_type LL.ctx :: List.map args ~f:box_type in
        let result_types = box_type result in
        function_type result_types (Array.of_list args_type)
    | Type.TypeVar _ -> failwith "Unknown type var"
    | Type.Custom _ -> pointer_type LL.ctx

  let lltype_ref (TypeMap map) = function
    | Type.Int -> i64_type LL.ctx
    | Type.Arrow (args, result) ->
        let box_type = function
          | Type.Int -> i64_type LL.ctx
          | Type.TypeVar id -> Map.find_exn map id
          | _ -> pointer_type LL.ctx
        in
        let args_type = pointer_type LL.ctx :: List.map args ~f:box_type in
        let result_types = box_type result in
        function_type result_types (Array.of_list args_type)
    | Type.TypeVar id ->
        let tpe = Map.find_exn map id in
        tpe
    | Type.Custom _ -> pointer_type LL.ctx
end

module BindingCtx = struct
  type t =
    | BindMap of {
        locals : (string, Bind.t, String.comparator_witness) Map.t;
        toplevels : (string, Bind.t, String.comparator_witness) Map.t;
        accessors : (string, Bind.t, String.comparator_witness) Map.t;
      }

  exception UnboundedName of string

  type def = Local of Bind.t | Top of Bind.t

  let find (BindMap { locals; toplevels; _ }) name =
    match Map.find locals name with
    | Some v -> Local v
    | None -> (
        match Map.find toplevels name with
        | Some v -> Top v
        | None -> raise (UnboundedName name))

  let local (BindMap { locals; _ }) name =
    match Map.find locals name with
    | Some v -> v
    | None -> raise (UnboundedName name)

  let accessor (BindMap { accessors; _ }) name =
    match Map.find accessors name with
    | Some v -> v
    | None -> raise (UnboundedName name)

  let add (BindMap bm) name bind =
    BindMap { bm with locals = Map.set bm.locals ~key:name ~data:bind }

  let append_locals (BindMap bm) new_locals =
    BindMap { bm with locals = Map.merge_disjoint_exn bm.locals new_locals }

  let replace_locals (BindMap { accessors; toplevels; _ }) l =
    BindMap
      { locals = Map.of_alist_exn (module String) l; toplevels; accessors }

  let of_maps ~accessors ~toplevels ~locals =
    BindMap { locals; toplevels; accessors }
end

module TagsCtx (LL : LowLevelCtx) = struct
  type t =
    | TagsCtx of {
        checkers : (string, Bind.t, String.comparator_witness) Map.t;
        layouts : (string, lltype, String.comparator_witness) Map.t;
      }

  let of_lists c l =
    TagsCtx
      {
        checkers = Map.of_alist_exn (module String) c;
        layouts = Map.of_alist_exn (module String) l;
      }

  let checker_tpe = function_type (i1_type LL.ctx) [| pointer_type LL.ctx |]

  let tag_checker (TagsCtx self) name =
    Bind.single_exn (Map.find_exn self.checkers name)

  let constructor_struct (TagsCtx self) name = Map.find_exn self.layouts name
end

module CompilerCtx (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)

  type t = {
    tag_ctx : TagMap.t;
    bind_ctx : BindingCtx.t;
    type_ctx : TypeMap.t;
    builder : llbuilder;
  }
end

module VarCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)

  let compile_var CC.{ bind_ctx; builder; _ } name =
    match BindingCtx.find bind_ctx name with
    | BindingCtx.Local local -> local
    | BindingCtx.Top top -> (
        let build_capture var_value =
          let fun_str_tpe = struct_type LL.ctx [| pointer_type LL.ctx |] in
          let instance = build_malloc fun_str_tpe (name ^ "_inst") builder in
          let _ = build_store var_value instance builder in
          instance
        in
        match top with
        | Versions v ->
            Versions (List.map v ~f:(fun (t, v) -> (t, build_capture v)))
        | Single s -> Single (build_capture s))
end

module ConstCompiler (LL : LowLevelCtx) = struct
  let compile_const value = Bind.Single (const_int (i64_type LL.ctx) value)
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
    let lvalue = Bind.single_exn (compiler lhs) in
    let rvalue = Bind.single_exn (compiler rhs) in
    Bind.Single (command lvalue rvalue name builder)
end

module ApplyCompiler (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module CC = CompilerCtx (LL)

  exception UnfoundRealization

  let find_realization type_ctx bind realization_type =
    match bind with
    | Bind.Single mono_fun -> mono_fun
    | Bind.Versions specializations -> (
        let type_repr = TypeMap.lltype_ref type_ctx realization_type in
        match
          List.find specializations ~f:(fun (fun_tpe, _) ->
              phys_equal fun_tpe type_repr)
        with
        | Some (_, found) -> found
        | None -> raise UnfoundRealization)

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

  let compile_apply compiler CC.{ builder; type_ctx; _ } fun_value arg_values
      spec_tpe =
    let fun_bind = compiler fun_value in
    (* replace single_exn with find_realization *)
    let arguments_values =
      List.map arg_values ~f:(fun arg -> Bind.single_exn (compiler arg))
    in
    let fun_realization = find_realization type_ctx fun_bind spec_tpe in
    let function_type = TypeMap.lltype_ref type_ctx spec_tpe in
    let call =
      boxed_call builder function_type fun_realization arguments_values
    in
    Bind.Single call
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
                    ("lcapture_" ^ name ^ "_ptr")
                    bd
                in
                let _ = build_store s field_ptr bd in
                ind + 1
            | Bind.Versions v ->
                let _ =
                  List.mapi v ~f:(fun ind (_, v) ->
                      let field_ptr =
                        build_struct_gep capture_context_type lambda_instance
                          ind
                          ("lcapture_" ^ name ^ "_ptr")
                          bd
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
              build_struct_gep cap.struct_tpe cap.value ind (name ^ "_ptr") bd
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
    let _ = build_ret (Bind.single_exn lambda_result) CC.(ctx.builder) in
    ()

  let compile_mono_lambda compiler ctx captures arguments body lambda_type =
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
    Bind.Single cap.value

  let compile_poly_lambda compiler ctx captures arguments result ids tpe =
    let variants = TypeMap.variants CC.(ctx.type_ctx) ids in
    let compiled_variants =
      List.map variants ~f:(fun c ->
          let new_context = CC.{ ctx with type_ctx = c } in
          compiler new_context
            (Texp.Lambda (captures, arguments, result, Mono tpe)))
    in
    let specialized =
      List.map2_exn variants compiled_variants ~f:(fun tc spec ->
          (TypeMap.lltype_ref tc tpe, Bind.single_exn spec))
    in
    Bind.Versions specialized
end

module BlockCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)

  let compile_block compiler ctx defs result =
    let compile_def Symbol.{ name; result } dctx =
      let result_value = compiler dctx result in
      let _ =
        match result_value with
        | Bind.Single s ->
            set_value_name name s;
            ()
        | Bind.Versions v ->
            List.iter v ~f:(fun (_, v) -> set_value_name name v);
            ()
      in
      (name, result_value)
    in
    let new_context =
      List.fold_left defs ~init:ctx ~f:(fun acc_ctx def ->
          let name, value = compile_def def acc_ctx in
          let new_context =
            CC.
              { ctx with bind_ctx = BindingCtx.add acc_ctx.bind_ctx name value }
          in
          new_context)
    in
    compiler new_context result
end

module FieldCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)

  let compile_field compiler CC.{ bind_ctx; builder; _ } str name =
    let str_val = Bind.single_exn (compiler str) in
    let accessor = Bind.single_exn (BindingCtx.accessor bind_ctx name) in
    let call =
      build_call (type_of accessor) accessor [| str_val |] name builder
    in
    Bind.Single call
end

module MatchCompiler (LL : LowLevelCtx) = struct
  open CompilerCtx (LL)
  module TagMap = TagsCtx (LL)

  let rec case_check tag_ctx dec ptr builder =
    match dec with
    | Deconstructor.Var _ -> const_int (i1_type LL.ctx) 1
    | Deconstructor.Constructor (name, sub_decs) ->
        let tag_checker = TagMap.tag_checker tag_ctx name in
        let constructor_check =
          build_call TagMap.checker_tpe tag_checker [| ptr |] (name ^ "_check")
            builder
        in
        let layout = TagMap.constructor_struct tag_ctx name in
        let field_types = struct_element_types layout in
        let sub_checks =
          List.mapi sub_decs ~f:(fun ind dec ->
              let field_type = field_types.(ind + 1) in
              let field =
                let ptr =
                  build_struct_gep layout ptr (ind + 1) "ptr_field" builder
                in
                build_load field_type ptr "value" builder
              in
              case_check tag_ctx dec field builder)
        in
        List.fold_left sub_checks ~init:constructor_check ~f:(fun acc x ->
            build_mul acc x "con_and" builder)

  let rec extract_values tag_ctx dec ptr builder =
    match dec with
    | Deconstructor.Var name ->
        Map.singleton (module String) name (Bind.Single ptr)
    | Deconstructor.Constructor (name, sub_decs) ->
        let layout = TagMap.constructor_struct tag_ctx name in
        let field_types = struct_element_types layout in
        let sub_values =
          List.mapi sub_decs ~f:(fun ind dec ->
              let field_type = field_types.(ind + 1) in
              let field =
                let ptr =
                  build_struct_gep layout ptr (ind + 1) "ptr_field" builder
                in
                build_load field_type ptr "value" builder
              in
              extract_values tag_ctx dec field builder)
        in
        List.fold_left sub_values
          ~init:(Map.empty (module String))
          ~f:(fun acc v -> Map.merge_disjoint_exn acc v)

  let compile_match compiler ctx obj cases =
    let obj = Bind.single_exn (compiler ctx obj) in
    let control_block = insertion_block ctx.builder in
    let par_fun = block_parent control_block in
    let match_blocks =
      List.map cases ~f:(fun _ -> append_block LL.ctx "branch" par_fun)
    in
    let unmatched_block = append_block LL.ctx "unmatched" par_fun in
    let block_ifelse =
      List.zip_exn match_blocks
        (List.tl_exn (List.append match_blocks [ unmatched_block ]))
    in
    let _, branch_results =
      List.fold2_exn cases block_ifelse ~init:(ctx.builder, [])
        ~f:(fun (builder, nodes) (deconstr, expr) (jmp, els) ->
          let condition = case_check ctx.tag_ctx deconstr obj builder in
          let _ = build_cond_br condition jmp els builder in
          let case_builder = builder_at_end LL.ctx jmp in
          let else_builder = builder_at_end LL.ctx els in
          let extract_names = extract_values ctx.tag_ctx deconstr obj builder in
          let result =
            Bind.single_exn
              (compiler
                 {
                   ctx with
                   builder = case_builder;
                   bind_ctx =
                     BindingCtx.append_locals ctx.bind_ctx extract_names;
                 }
                 expr)
          in
          (else_builder, (result, jmp) :: nodes))
    in
    let phi_node = build_phi branch_results "match_result" ctx.builder in
    Bind.Single phi_node
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
    | Texp.Var (name, _) -> VarC.compile_var ctx name
    | Texp.Const (value, _) -> ConstC.compile_const value
    | Texp.Oper (bop, lhs, rhs, _) ->
        OperC.compile_operator (compile_expression ctx) ctx bop lhs rhs
    | Texp.Apply (fu, arguments, _, spec_tpe) ->
        ApplyC.compile_apply (compile_expression ctx) ctx fu arguments
          (Type.monotype spec_tpe)
    | Texp.Lambda (captures, arguments, result, Quant (ids, tpe)) ->
        LambdaC.compile_poly_lambda compile_expression ctx captures arguments
          result ids tpe
    | Texp.Lambda (captures, arguments, result, Mono lambda_type) ->
        LambdaC.compile_mono_lambda compile_expression ctx captures arguments
          result lambda_type
    | Texp.PatMatch (obj, capture, _) ->
        PatMatchC.compile_match compile_expression ctx obj capture
    | Texp.Block (defs, result) ->
        BlockC.compile_block compile_expression ctx defs result
    | Texp.Field (str, name, _) ->
        FieldC.compile_field (compile_expression ctx) ctx str name
end

module TypeCompiler (LL : LowLevelCtx) = struct
  type sum = {
    checkers : (string * Bind.t) list;
    layouts : (string * lltype) list;
  }

  type record = {
    constructor : string * Bind.t;
    accessors : (string * Bind.t) list;
  }

  let compile_sum constructors =
    let open Parsing.Ast.Symbol in
    let count = List.length constructors in
    let tag_type =
      if count <= 2 then i1_type LL.ctx
      else if count <= 256 then i8_type LL.ctx
      else if count <= 65536 then i16_type LL.ctx
      else i32_type LL.ctx
    in
    let layouts =
      List.map constructors ~f:(fun con ->
          let fields =
            List.map con.fields ~f:(fun (_, tpe) ->
                match tpe with
                | Type.Int -> i64_type LL.ctx
                | _ -> pointer_type LL.ctx)
          in
          (con.name, struct_type LL.ctx (Array.of_list (tag_type :: fields))))
    in
    let checkers =
      List.mapi constructors ~f:(fun ind con ->
          let checker_type =
            function_type (i1_type LL.ctx) [| pointer_type LL.ctx |]
          in
          let checker_fun = define_function con.name checker_type LL.md in
          let builder = builder_at_end LL.ctx (entry_block checker_fun) in
          let instance = (params checker_fun).(0) in
          let tag_ptr =
            build_struct_gep
              (struct_type LL.ctx [| tag_type |])
              instance ind "tag_ptr" builder
          in
          let tag = build_load tag_type tag_ptr "tag_val" builder in
          let cmp =
            build_icmp Icmp.Eq tag (const_int tag_type ind) "result" builder
          in
          let _ = build_ret cmp builder in
          (con.name, Bind.Single checker_fun))
    in
    { checkers; layouts }

  let compile_record constructor =
    let open Parsing.Ast.Symbol in
    let fields_types =
      Array.of_list
        (List.map constructor.fields ~f:(fun (_, tpe) ->
             match tpe with
             | Type.Int -> i64_type LL.ctx
             | _ -> pointer_type LL.ctx))
    in
    let struct_tpe = struct_type LL.ctx fields_types in
    let constructor_fun =
      let constructor_ty = function_type (pointer_type LL.ctx) fields_types in
      let f = define_function constructor.name constructor_ty LL.md in
      let bd = builder_at_end LL.ctx (entry_block f) in
      let instance = build_malloc struct_tpe "instance" bd in
      let _ =
        Array.iteri (params f) ~f:(fun i field_value ->
            let field_ptr =
              build_struct_gep struct_tpe instance i "field_ptr" bd
            in
            let _ = build_store field_value field_ptr bd in
            ())
      in
      let _ = build_ret instance bd in
      (constructor.name, Bind.Single f)
    in
    let accessors =
      Array.mapi
        (Array.zip_exn (Array.of_list constructor.fields) fields_types)
        ~f:(fun i ((name, _), llt) ->
          let accessor_ty = function_type llt [| pointer_type LL.ctx |] in
          let f = define_function ("get_" ^ name) accessor_ty LL.md in
          let bd = builder_at_end LL.ctx (entry_block f) in
          let instance = (params f).(0) in
          let field_ptr =
            build_struct_gep struct_tpe instance i (name ^ "_ptr") bd
          in
          let field_value = build_load llt field_ptr name bd in
          let _ = build_ret field_value bd in
          (name, Bind.Single f))
    in
    { constructor = constructor_fun; accessors = List.of_array accessors }
end

module TopLevelCompiler (LL : LowLevelCtx) = struct
  module CC = CompilerCtx (LL)
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)
  module ExpComp = ExpressionCompiler (LL)
  module TypeComp = TypeCompiler (LL)

  let compile_function ~toplevels ~accessors ~layouts ~checkers
      Texp.TypedProgram.{ name; arguments; expr; _ } =
    let fun_val = Bind.single_exn (Map.find_exn toplevels name) in
    let builder = builder_at_end LL.ctx (entry_block fun_val) in
    let context =
      let arguments_values = Array.to_list (params fun_val) in
      let _ =
        List.iter2 arguments arguments_values ~f:(fun name value ->
            set_value_name name value)
      in
      let arguments_bindings =
        Map.of_alist_exn
          (module String)
          (List.map2_exn ("ctx" :: arguments) arguments_values ~f:(fun name v ->
               (name, Bind.Single v)))
      in
      let initial_bindings =
        BindingCtx.of_maps ~locals:arguments_bindings ~accessors ~toplevels
      in
      CC.
        {
          bind_ctx = initial_bindings;
          type_ctx = TypeMap.empty;
          tag_ctx = TagMap.of_lists checkers layouts;
          builder;
        }
    in
    let body = ExpComp.compile_expression context expr in
    build_ret (Bind.single_exn body) builder

  type compiled_type = Record of TypeComp.record | Sum of TypeComp.sum

  let compile_type Symbol.{ constructors; _ } =
    match constructors with
    | [ single ] -> Record (TypeComp.compile_record single)
    | multi -> Sum (TypeComp.compile_sum multi)
end

module ProgramCompiler = struct
  module LL : LowLevelCtx = struct
    let ctx = create_context ()
    let md = create_module ctx "example"
  end

  module TypeMap = PolyCtx (LL)
  module TopComp = TopLevelCompiler (LL)

  let compile_program _ program =
    let open Texp.TypedProgram in
    let constructors, accessors, checkers, layouts =
      List.fold_left program.types ~init:([], [], [], [])
        ~f:(fun
            (constructors_acc, accessors_acc, checkers_acc, layouts_acc) t ->
          match TopComp.compile_type t with
          | Record { constructor; accessors } ->
              ( constructor :: constructors_acc,
                List.append accessors accessors_acc,
                checkers_acc,
                layouts_acc )
          | Sum { checkers; layouts } ->
              ( constructors_acc,
                accessors_acc,
                List.append checkers checkers_acc,
                List.append layouts layouts_acc ))
    in
    let toplevels =
      let declare_top { name; fun_type; _ } =
        let fun_ty = TypeMap.lltype_no_ctx (Type.monotype fun_type) in
        let fun_def = define_function name fun_ty LL.md in
        (name, Bind.Single fun_def)
      in
      Map.of_alist_exn
        (module String)
        (List.append (List.map program.functions ~f:declare_top) constructors)
    in
    let accessors = Map.of_alist_exn (module String) accessors in
    let _ =
      List.map program.functions
        ~f:(TopComp.compile_function ~toplevels ~accessors ~layouts ~checkers)
    in
    LL.md
end

let dump_object_file md filename =
  Llvm_all_backends.initialize ();
  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let cpu = "generic" in
  let reloc_mode = Llvm_target.RelocMode.Default in
  let machine =
    Llvm_target.TargetMachine.create ~triple:target_triple ~cpu ~reloc_mode
      target
  in
  let data_layout =
    Llvm_target.TargetMachine.data_layout machine
    |> Llvm_target.DataLayout.as_string
  in
  Llvm.set_target_triple target_triple md;
  Llvm.set_data_layout data_layout md;
  let filename = filename in
  let file_type = Llvm_target.CodeGenFileType.ObjectFile in
  Llvm_analysis.assert_valid_module md;
  Llvm_target.TargetMachine.emit_to_file md file_type filename machine;
  ()
