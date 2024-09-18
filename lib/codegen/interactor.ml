open Core
open Llvm
open Context
open Analys.Address.Interactor
open Analys.Address
open Analys.Capture
open Actor
open Typing
open Expression

module InteractorCompiler (LL : LowLevelCtx) = struct
  module TypeCtx = PolyCtx (LL)
  module TagsCtx = TagsCtx (LL)
  module LocCtx = CompilerCtx (LL)
  module TopCtx = TopLevelCtx (LL)
  module ActorComp = ActorCompiler (LL)
  module ExpressionComp = ExpressionCompiler (LL)

  let actor_type = pointer_type LL.ctx
  let me_type = pointer_type LL.ctx

  let compile_handler top_ctx interactor_def actor handler builder =
    let interactor_function = block_parent (insertion_block builder) in
    let instance = param interactor_function 0 in
    let message = param interactor_function 1 in
    let me = param interactor_function 2 in
    let state_info =
      ActorContext.state_layout TopCtx.(top_ctx.actor_ctx) handler.state
    in
    let fields_bindings =
      List.mapi state_info.fields ~f:(fun ind (name, tpe) ->
          let field_ptr =
            build_struct_gep state_info.layout instance (ind + 1)
              (name ^ ".ptr") builder
          in
          (name, Bind.Single (build_load tpe field_ptr name builder)))
    in
    let locals =
      ("me", Bind.Single me)
      :: ("message", Bind.Single message)
      :: fields_bindings
    in
    let context = TopCtx.to_local top_ctx locals builder in
    let _ =
      List.fold handler.body ~init:context ~f:(fun acc_ctx statement ->
          match statement with
          | Val { name; result } ->
              let value = ExpressionComp.compile_expression acc_ctx result in
              Bind.set_name value name;
              LocCtx.
                {
                  acc_ctx with
                  bind_ctx = BindingCtx.add name value acc_ctx.bind_ctx;
                }
          | Mutate new_state ->
              let new_state =
                ExpressionComp.compile_expression acc_ctx new_state
              in
              let mutator =
                ActorContext.mutator TopCtx.(top_ctx.actor_ctx) actor
              in
              let _ =
                build_call ActorComp.mutate_type mutator
                  [| instance; Bind.single_exn new_state |]
                  "" acc_ctx.builder
              in
              acc_ctx
          | Spawn { name; actor } ->
              let new_actor = ExpressionComp.compile_expression acc_ctx actor in
              Bind.set_name new_actor name;
              let new_mailbox =
                Runtime.build_spawn acc_ctx.runtime
                  (Bind.single_exn new_actor)
                  "spawn.mailbox" acc_ctx.builder
              in
              LocCtx.
                {
                  acc_ctx with
                  bind_ctx = BindingCtx.add name (Bind.Single new_mailbox) acc_ctx.bind_ctx;
                }
          | Send { message; mail; destination } ->
              let message_value =
                Bind.single_exn
                  (ExpressionComp.compile_expression acc_ctx message)
              in
              let mail_value =
                Bind.single_exn (ExpressionComp.compile_expression acc_ctx mail)
              in
              let handler = Map.find_exn interactor_def destination in
              let _ =
                Runtime.build_send acc_ctx.runtime
                  (Type.monotype (Cexp.type_of message))
                  mail_value handler message_value "" acc_ctx.builder
              in

              acc_ctx)
    in
    let _ = build_ret_void builder in
    ()

  let rec message_type_label = function
    | Type.Actor name -> "act_" ^ name
    | Type.Int -> "i64"
    | Type.Float -> "f64"
    | Type.Bool -> "b"
    | Type.MailBox name -> "mail_" ^ name
    | Type.Named name -> name
    | Type.TypeVar id -> "tv_" ^ string_of_int id
    | Type.Arrow (args, result) ->
        String.concat ~sep:"_" (List.map args ~f:message_type_label)
        ^ "_arrow_" ^ message_type_label result
    | Type.Operator (name, args) ->
        name ^ "_"
        ^ String.concat ~sep:"_" (List.map args ~f:message_type_label)

  let generate_blocks handlers par_fun =
    let checkers =
      List.map handlers ~f:(fun h ->
          append_block LL.ctx (h.state ^ ".dispatch") par_fun)
    in
    let branches =
      List.map handlers ~f:(fun h ->
          append_block LL.ctx (h.state ^ ".handlers") par_fun)
    in
    (checkers, branches)

  let compile_interactor top_ctx interactor_def interactor =
    let interactor_function = Map.find_exn interactor_def interactor.id in
    let actor = param interactor_function 0 in
    let checkers, branches =
      generate_blocks interactor.handlers interactor_function
    in
    let entry_builder =
      builder_at_end LL.ctx (entry_block interactor_function)
    in
    let _ = build_br (List.hd_exn checkers) entry_builder in
    let rec loop handlers checkers branches =
      match (handlers, checkers, branches) with
      | ( handler :: other_handlers,
          checker :: next_checker :: other_checkers,
          branch :: other_branches ) ->
          let tag_checker =
            TagsCtx.tag_checker TopCtx.(top_ctx.tag_ctx) handler.state
          in
          let checker_builder = builder_at_end LL.ctx checker in
          let condition =
            build_call TagsCtx.checker_tpe tag_checker [| actor |]
              (handler.state ^ ".condition")
              checker_builder
          in
          let _ = build_cond_br condition branch next_checker checker_builder in
          let handler_builder = builder_at_end LL.ctx branch in
          let _ =
            compile_handler top_ctx interactor_def interactor.actor handler
              handler_builder
          in
          loop other_handlers (next_checker :: other_checkers) other_branches
      | [ handler ], [ checker ], [ branch ] ->
          let checker_builder = builder_at_end LL.ctx checker in
          let _ = build_br branch checker_builder in
          let handler_builder = builder_at_end LL.ctx branch in
          let _ =
            compile_handler top_ctx interactor_def interactor.actor handler
              handler_builder
          in
          ()
      | _ -> ()
    in
    loop interactor.handlers checkers branches

  let compile_interactors top_ctx interactors =
    let interactor_def =
      Map.of_alist_exn
        (module Interactor.Id)
        (List.map interactors ~f:(fun interactor ->
             let message_tpe =
               TypeCtx.lltype_no_ctx (message_type interactor)
             in
             let interactor_type =
               function_type (void_type LL.ctx)
                 [| actor_type; message_tpe; me_type |]
             in
             ( interactor.id,
               define_function
                 (interactor.actor ^ "."
                 ^ message_type_label (message_type interactor)
                 ^ ".interactor")
                 interactor_type LL.md )))
    in
    List.map interactors ~f:(fun interactor ->
        compile_interactor top_ctx interactor_def interactor)
end
