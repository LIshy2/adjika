open Core
open Llvm
open Context
open Typing
open Expression

module FunctionCompiler (LL : LowLevelCtx) = struct
  module LC = CompilerCtx (LL)
  module TLC = TopLevelCtx (LL)
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)
  module ExpComp = ExpressionCompiler (LL)

  let argument_types = function
    | Type.Arrow (args, _) -> args
    | _ -> failwith "Not function type"

  let compile_mono_function top_ctx tm fun_decl arguments expr arrow_tpe =
    let builder = builder_at_end LL.ctx (entry_block fun_decl) in
    let context =
      let arguments_values = Array.to_list (params fun_decl) in
      let _ =
        List.iter2 ("ctx" :: arguments) arguments_values ~f:(fun name value ->
            set_value_name name value)
      in
      let arguments_bindings =
        List.map2_exn ("ctx" :: arguments) arguments_values ~f:(fun name v ->
            (name, Bind.Single v))
      in
      TLC.to_local_with_types top_ctx arguments_bindings tm builder
    in
    let resources =
      let arg_types = argument_types arrow_tpe in
      let arg_values = List.tl_exn (Array.to_list (params fun_decl)) in
      Scope.Resources.of_list
        (List.map2_exn arg_types arg_values ~f:(fun t v ->
             Scope.Resources.obj v (TypeMap.type_to_bind tm t)))
    in
    let body = ExpComp.compile_expression context expr in
    let _ = Scope.desctruct resources top_ctx.runtime builder in
    let _ = build_ret body context.builder in
    ()

  let compile_poly_function top_ctx quants name arguments expr tpe =
    let decl =
      Bind.versions_exn (BindingCtx.toplevel TLC.(top_ctx.bind_ctx) name)
    in
    List.iter2_exn (TypeMap.variants TypeMap.empty quants) decl
      ~f:(fun tm (_, fun_decl) ->
        compile_mono_function top_ctx tm fun_decl arguments expr tpe)

  let compile_function top_ctx
      Texp.TProgram.{ name; arguments; fun_type; expr; _ } =
    match fun_type with
    | Type.Mono tpe ->
        compile_mono_function top_ctx TypeMap.empty
          (Bind.single_exn (BindingCtx.toplevel TLC.(top_ctx.bind_ctx) name))
          arguments expr tpe
    | Type.Quant (quants, tpe) ->
        compile_poly_function top_ctx quants name arguments expr tpe

  let declare_function Texp.TProgram.{ name; fun_type; _ } =
    match fun_type with
    | Type.Mono t ->
        let fun_ty = TypeMap.lltype_no_ctx t in
        let fun_def = define_function name fun_ty LL.md in
        Bind.Single fun_def
    | Type.Quant (quants, t) ->
        Bind.Versions
          (List.map (TypeMap.variants TypeMap.empty quants) ~f:(fun tm ->
               let fun_ty = TypeMap.lltype_ref tm t in
               let fun_def = define_function name fun_ty LL.md in
               (TypeMap.type_to_bind tm t, fun_def)))
end
