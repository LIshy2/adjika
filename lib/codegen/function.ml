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

  let compile_mono_function top_ctx tm fun_decl arguments expr =
    let builder = builder_at_end LL.ctx (entry_block fun_decl) in
    let context =
      let arguments_values = Array.to_list (params fun_decl) in
      let _ =
        List.iter2 arguments arguments_values ~f:(fun name value ->
            set_value_name name value)
      in
      let arguments_bindings =
        List.map2_exn ("ctx" :: arguments) arguments_values ~f:(fun name v ->
            (name, Bind.Single v))
      in
      TLC.to_local_with_types top_ctx arguments_bindings tm builder
    in
    let body = ExpComp.compile_expression context expr in
    let _ = build_ret (Bind.single_exn body) context.builder in
    ()

  let compile_poly_function top_ctx quants name arguments expr =
    let decl =
      Bind.versions_exn (BindingCtx.toplevel TLC.(top_ctx.bind_ctx) name)
    in
    List.iter2_exn (TypeMap.variants TypeMap.empty quants) decl
      ~f:(fun tm (_, fun_decl) ->
        compile_mono_function top_ctx tm fun_decl arguments expr)

  let compile_function top_ctx
      Texp.TProgram.{ name; arguments; fun_type; expr; _ } =
    match fun_type with
    | Type.Mono _ ->
        compile_mono_function top_ctx TypeMap.empty
          (Bind.single_exn (BindingCtx.toplevel TLC.(top_ctx.bind_ctx) name))
          arguments expr
    | Type.Quant (quants, _) ->
        compile_poly_function top_ctx quants name arguments expr

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
