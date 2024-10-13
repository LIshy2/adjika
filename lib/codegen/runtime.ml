open Llvm
open Typing

type t = {
  send_int : llvalue;
  send_int_type : lltype;
  send_ptr : llvalue;
  send_ptr_type : lltype;
  spawn_actor : llvalue;
  spawn_actor_type : lltype;
  allocate : llvalue;
  allocate_type : lltype;
  dup : llvalue;
  dup_type : lltype;
  dedup : llvalue;
  dedup_type : lltype;
}

let declare_in_module ctx md =
  let send_int_type =
    function_type (void_type ctx)
      [| i64_type ctx; pointer_type ctx; pointer_type ctx |]
  in
  let send_ptr_type =
    function_type (void_type ctx)
      [| pointer_type ctx; pointer_type ctx; pointer_type ctx |]
  in
  let spawn_actor_type =
    function_type (pointer_type ctx) [| pointer_type ctx |]
  in
  let allocate_type = function_type (pointer_type ctx) [| pointer_type ctx |] in
  let dup_type = function_type (pointer_type ctx) [| pointer_type ctx |] in
  let dedup_type = function_type (pointer_type ctx) [| pointer_type ctx |] in
  {
    send_int = declare_function "send_int" send_int_type md;
    send_int_type;
    send_ptr = declare_function "send_ptr" send_ptr_type md;
    send_ptr_type;
    spawn_actor = declare_function "spawn_actor" spawn_actor_type md;
    spawn_actor_type;
    allocate = declare_function "allocate" allocate_type md;
    allocate_type;
    dup = declare_function "dup" dup_type md;
    dup_type;
    dedup = declare_function "dedup" dedup_type md;
    dedup_type;
  }

let init_interactor_body ctx fnty api_fun original_fun =
  let builder = builder_at_end ctx (entry_block api_fun) in
  let _ = build_call fnty original_fun (params api_fun) "" builder in
  let _ = build_ret_void builder in
  ()

let init_constructor_body ctx fnty api_fun original_fun =
  let builder = builder_at_end ctx (entry_block api_fun) in
  let result = build_call fnty original_fun (params api_fun) "result" builder in
  let _ = build_ret result builder in
  ()

let declare_main ctx md =
  let handler_type =
    function_type (void_type ctx)
      [| pointer_type ctx; pointer_type ctx; pointer_type ctx |]
  in
  let initial_handler =
    declare_function "Main.Start.interactor" handler_type md
  in
  let api_handler = define_function "init_main_interactor" handler_type md in
  init_interactor_body ctx handler_type api_handler initial_handler;
  let constructor_type = function_type (pointer_type ctx) [||] in
  let initial_constructor =
    declare_function "Initing.constructor" constructor_type md
  in
  let api_constructor = define_function "init_main_state" constructor_type md in
  init_constructor_body ctx constructor_type api_constructor initial_constructor;
  ()

let build_send_int runtime mailbox handler message builder =
  build_call runtime.send_int_type runtime.send_int
    [| message; handler; mailbox |]
    builder

let build_send_ptr runtime mailbox handler message builder =
  build_call runtime.send_ptr_type runtime.send_ptr
    [| message; handler; mailbox |]
    builder

let build_send runtime tpe mailbox handler message builder =
  match tpe with
  | Type.Int -> build_send_int runtime mailbox handler message builder
  | _ -> build_send_ptr runtime mailbox handler message builder

let build_spawn runtime state =
  build_call runtime.spawn_actor_type runtime.spawn_actor [| state |]

let build_allocate runtime layout_ptr =
  build_call runtime.allocate_type runtime.allocate [| layout_ptr |]

let build_dup runtime obj = build_call runtime.dup_type runtime.dup [| obj |]

let build_dedup runtime obj =
  build_call runtime.dedup_type runtime.dedup [| obj |]
