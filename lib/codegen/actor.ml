open Core
open Llvm
open Llvm_target
open Context
open Parsing.Ast.Actor

module ActorCompiler (LL : LowLevelCtx) = struct
  module TypeCtx = PolyCtx (LL)

  let runtime_state_tag_type actor =
    let count = List.length actor.states in
    if count <= 2 then i1_type LL.ctx
    else if count <= 256 then i8_type LL.ctx
    else if count <= 65536 then i16_type LL.ctx
    else i32_type LL.ctx

  let mutate_type =
    function_type (void_type LL.ctx)
      [| pointer_type LL.ctx; pointer_type LL.ctx |]

  let state_struct tag state =
    struct_type LL.ctx
      (Array.of_list
         (tag
         :: List.map state.fields ~f:(fun (_, tpe) -> TypeCtx.lltype_no_ctx tpe)
         ))

  let size_of_type t = DataLayout.abi_size t LL.data_layout

  let actor_alloc_struct actor =
    let tag = runtime_state_tag_type actor in
    let structs = List.map actor.states ~f:(state_struct tag) in
    Option.value_exn
      (List.max_elt structs ~compare:(fun lhs rhs ->
           Int64.compare (size_of_type lhs) (size_of_type rhs)))

  let compile_state_constructor layout state state_ind =
    let fields =
      List.map state.fields ~f:(fun (_, t) -> TypeCtx.lltype_no_ctx t)
    in
    let constructor_type =
      function_type (pointer_type LL.ctx) (Array.of_list fields)
    in
    let constructor_fun =
      define_function (state.name ^ ".constructor") constructor_type LL.md
    in
    let bd = builder_at_end LL.ctx (entry_block constructor_fun) in
    let instance = build_malloc layout "instance" bd in
    let tag_type = (struct_element_types layout).(0) in
    let tag_ptr = build_struct_gep layout instance 0 "tag.ptr" bd in
    let _ = build_store (const_int tag_type state_ind) tag_ptr bd in
    let _ =
      Array.iteri (params constructor_fun) ~f:(fun i field_value ->
          let field_ptr =
            build_struct_gep layout instance (i + 1) "field.ptr" bd
          in
          let _ = build_store field_value field_ptr bd in
          ())
    in
    let _ = build_ret instance bd in
    constructor_fun

  let compile_state_checker layout state state_ind =
    let _ = state.fields in
    let checker_type =
      function_type (i1_type LL.ctx) [| pointer_type LL.ctx |]
    in
    let checker_fun =
      define_function (state.name ^ ".checker") checker_type LL.md
    in
    let builder = builder_at_end LL.ctx (entry_block checker_fun) in
    let instance = (params checker_fun).(0) in
    let tag_type = (struct_element_types layout).(0) in
    let tag_ptr =
      build_struct_gep
        (struct_type LL.ctx [| tag_type |])
        instance 0 "tag_ptr" builder
    in
    let tag = build_load tag_type tag_ptr "tag_val" builder in
    let cmp =
      build_icmp Icmp.Eq tag (const_int tag_type state_ind) "result" builder
    in
    let _ = build_ret cmp builder in
    checker_fun

  let compile_mutator layout actor =
    let mutate_fun =
      define_function (actor.name ^ ".mutator") mutate_type LL.md
    in
    let builder = builder_at_end LL.ctx (entry_block mutate_fun) in
    let source = (params mutate_fun).(0) in
    let destination = (params mutate_fun).(1) in
    let _ =
      Array.iteri (struct_element_types layout) ~f:(fun i field_type ->
          let source_ptr =
            build_struct_gep layout source i "src_field.ptr" builder
          in
          let destination_ptr =
            build_struct_gep layout destination i "dst_field.ptr" builder
          in
          let destination_val =
            build_load field_type destination_ptr "dst_field.value" builder
          in
          let _ = build_store destination_val source_ptr builder in
          ())
    in
    let _ = build_ret_void builder in
    mutate_fun

  type actor = {
    state_info : (string * ActorContext.state_info) list;
    tag : lltype;
    constructors : (string * Bind.t) list;
    mutator : llvalue;
    checkers : (string * Bind.t) list;
  }

  let compile_actor actor =
    let tag = runtime_state_tag_type actor in
    let state_info =
      List.map actor.states ~f:(fun state ->
          let tag = runtime_state_tag_type actor in
          let layout = state_struct tag state in
          let fields =
            List.map state.fields ~f:(fun (name, t) ->
                (name, TypeCtx.lltype_no_ctx t))
          in
          (state.name, ActorContext.state layout fields))
    in
    let layout = actor_alloc_struct actor in
    let constructors =
      List.mapi actor.states ~f:(fun ind state ->
          ( state.name,
            Bind.Single
              (compile_state_constructor (state_struct tag state) state ind) ))
    in
    let mutator = compile_mutator layout actor in
    let checkers =
      List.mapi actor.states ~f:(fun ind state ->
          (state.name, Bind.Single (compile_state_checker layout state ind)))
    in
    { tag; state_info; constructors; mutator; checkers }
end
