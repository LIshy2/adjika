open Core
open Llvm
open Llvm_target
open Context
open Datatype
open Expression
open Actor
open Function
open Interactor
open Analys.Address

module TopLevelCompiler (LL : LowLevelCtx) = struct
  module LC = CompilerCtx (LL)
  module TLC = TopLevelCtx (LL)
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)
  module ExpComp = ExpressionCompiler (LL)
  module TypeComp = TypeCompiler (LL)
  module FunComp = FunctionCompiler (LL)
  module ActorComp = ActorCompiler (LL)
  module InteractorComp = InteractorCompiler (LL)

  let compile_function ctx fn = FunComp.compile_function ctx fn
  let compile_type runtime tpe = TypeComp.compile_type runtime tpe
  let compile_actor actor = ActorComp.compile_actor actor

  let compile_interactor ctx interactors =
    InteractorComp.compile_interactors ctx interactors
end

module PartialEnv (LL : LowLevelCtx) = struct
  module TLC = TopLevelCtx (LL)
  module TagMap = TagsCtx (LL)

  type context = {
    toplevels : (string * Bind.t) list;
    accessors : (string * Bind.t) list;
    checkers : (string * Bind.t) list;
    tag_types : (string * lltype) list;
    actor_states : (string * ActorContext.state_info) list;
    actor_mutators : (string * llvalue) list;
  }

  let concat lhs rhs =
    {
      toplevels = List.append lhs.toplevels rhs.toplevels;
      accessors = List.append lhs.accessors rhs.accessors;
      checkers = List.append lhs.checkers rhs.checkers;
      tag_types = List.append lhs.tag_types rhs.tag_types;
      actor_states = List.append lhs.actor_states rhs.actor_states;
      actor_mutators = List.append lhs.actor_mutators rhs.actor_mutators;
    }

  let empty =
    {
      toplevels = [];
      accessors = [];
      checkers = [];
      tag_types = [];
      actor_states = [];
      actor_mutators = [];
    }

  module Syntax = struct
    let ( ++ ) lhs rhs = concat lhs rhs

    let finalize
        {
          toplevels;
          accessors;
          checkers;
          tag_types;
          actor_states;
          actor_mutators;
        } runtime =
      TLC.
        {
          tag_ctx = TagMap.of_lists checkers tag_types;
          bind_ctx = BindingCtx.of_lists ~accessors ~toplevels ~locals:[];
          actor_ctx = ActorContext.of_lists actor_states actor_mutators;
          runtime;
        }

    let new_function name value = { empty with toplevels = [ (name, value) ] }

    let new_constructor name value =
      { empty with toplevels = [ (name, value) ] }

    let new_constructors constructors =
      List.fold_left constructors ~init:empty ~f:(fun acc (name, value) ->
          acc ++ new_constructor name value)

    let new_accessor name value = { empty with accessors = [ (name, value) ] }

    let new_accessors accessors =
      List.fold_left accessors ~init:empty ~f:(fun acc (name, value) ->
          acc ++ new_accessor name value)

    let new_checker name value = { empty with checkers = [ (name, value) ] }

    let new_checkers checkers =
      List.fold_left checkers ~init:empty ~f:(fun acc (name, value) ->
          acc ++ new_checker name value)

    let new_tag_type name value = { empty with tag_types = [ (name, value) ] }

    let new_tag_types tags =
      List.fold_left tags ~init:empty ~f:(fun acc (name, value) ->
          acc ++ new_tag_type name value)

    let new_mutator actor mutator =
      { empty with actor_mutators = [ (actor, mutator) ] }

    let new_actor_state name value =
      { empty with actor_states = [ (name, value) ] }

    let new_actor_states states =
      List.fold_left states ~init:empty ~f:(fun acc (name, value) ->
          acc ++ new_actor_state name value)
  end
end

module ProgramCompiler = struct
  let ll name =
    (module struct
      let ctx = create_context ()
      let md = create_module ctx name
      let data_layout = DataLayout.of_string (data_layout md)
    end : LowLevelCtx)

  let compile_program name program =
    let (module LL) = ll name in
    let module TypeMap = PolyCtx (LL) in
    let module TLC = TopLevelCtx (LL) in
    let module TopComp = TopLevelCompiler (LL) in
    let module ActorComp = ActorCompiler (LL) in
    let module FunComp = FunctionCompiler (LL) in
    let module PE = PartialEnv (LL) in
    let open PE.Syntax in
    let open AProgram in
    let runtime = Runtime.declare_in_module LL.ctx LL.md in
    let types_env =
      List.fold_left program.types ~init:PE.empty ~f:(fun acc t ->
          match TopComp.compile_type runtime t with
          | Record { constructor; accessors } ->
              let name, value = constructor in
              acc ++ new_constructor name value ++ new_accessors accessors
          | Sum { checkers; constructors; tags } ->
              acc
              ++ new_constructors constructors
              ++ new_checkers checkers ++ new_tag_types tags)
    in
    let actor_env =
      List.fold_left program.actors ~init:types_env ~f:(fun acc actor ->
          let ActorComp.{ checkers; constructors; mutator; state_info; tag } =
            TopComp.compile_actor actor
          in
          acc
          ++ new_actor_states state_info
          ++ new_tag_type actor.name tag
          ++ new_mutator actor.name mutator
          ++ new_checkers checkers
          ++ new_constructors constructors)
    in
    let functions_env =
      List.fold_left program.functions ~init:actor_env ~f:(fun acc typed_fun ->
          acc
          ++ new_function typed_fun.name (FunComp.declare_function typed_fun))
    in
    let ctx = finalize functions_env runtime in
    let _ = List.map program.functions ~f:(TopComp.compile_function ctx) in
    let _ = TopComp.compile_interactor ctx program.interactors in
    Runtime.declare_main LL.ctx LL.md;
    LL.md
end

module Object = struct
  let dump_object_file filename md =
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
end
