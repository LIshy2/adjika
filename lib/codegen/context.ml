open Core
open Llvm
open Typing
open Llvm_target

module type LowLevelCtx = sig
  val ctx : llcontext
  val md : llmodule
  val data_layout : DataLayout.t
end

module PolyCtx (LL : LowLevelCtx) = struct
  type spec_type = Int64 | Ptr

  let spec_to_lltype = function
    | Int64 -> i64_type LL.ctx
    | Ptr -> pointer_type LL.ctx

  let spec_to_bind_type = function
    | Int64 -> Bind.BindType.Int
    | Ptr -> Bind.BindType.Ptr

  type t = TypeMap of (int, spec_type, Int.comparator_witness) Map.t

  let empty = TypeMap (Map.empty (module Int))

  let variant (TypeMap map) id =
    [
      TypeMap (Map.add_exn map ~key:id ~data:Int64);
      TypeMap (Map.add_exn map ~key:id ~data:Ptr);
    ]

  let rec variants tm = function
    | id :: other ->
        let prev_ctx = variants tm other in
        List.bind prev_ctx ~f:(fun ctx -> variant ctx id)
    | [] -> [ tm ]

  let rec type_to_bind (TypeMap map) = function
    | Type.Int -> Bind.BindType.Int
    | Type.Arrow (args, result) ->
        Bind.BindType.Arrow
          ( List.map args ~f:(type_to_bind (TypeMap map)),
            type_to_bind (TypeMap map) result )
    | Type.TypeVar id ->
        let tpe = Map.find_exn map id in
        spec_to_bind_type tpe
    | Type.Named name -> Bind.BindType.Named name
    | Type.Operator (name, args) ->
        Bind.BindType.Operator
          (name, List.map args ~f:(type_to_bind (TypeMap map)))
    | Type.Actor name -> Bind.BindType.Actor name
    | Type.MailBox mail -> Bind.BindType.MailBox mail

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
    | Type.Named _ -> pointer_type LL.ctx
    | Type.Operator (_, _) -> pointer_type LL.ctx
    | Type.Actor _ -> pointer_type LL.ctx
    | Type.MailBox _ -> pointer_type LL.ctx

  let lltype_ref (TypeMap map) = function
    | Type.Int -> i64_type LL.ctx
    | Type.Arrow (args, result) ->
        let box_type = function
          | Type.Int -> i64_type LL.ctx
          | Type.TypeVar id -> spec_to_lltype (Map.find_exn map id)
          | _ -> pointer_type LL.ctx
        in
        let args_type = pointer_type LL.ctx :: List.map args ~f:box_type in
        let result_types = box_type result in
        function_type result_types (Array.of_list args_type)
    | Type.TypeVar id -> spec_to_lltype (Map.find_exn map id)
    | Type.Named _ -> pointer_type LL.ctx
    | Type.Operator (_, _) -> pointer_type LL.ctx
    | Type.Actor _ -> pointer_type LL.ctx
    | Type.MailBox _ -> pointer_type LL.ctx

  let lltype_elem (TypeMap map) = function
    | Type.Int -> i64_type LL.ctx
    | Type.Arrow (_, _) -> pointer_type LL.ctx
    | Type.TypeVar id -> spec_to_lltype (Map.find_exn map id)
    | Type.Named _ -> pointer_type LL.ctx
    | Type.Operator (_, _) -> pointer_type LL.ctx
    | Type.Actor _ -> pointer_type LL.ctx
    | Type.MailBox _ -> pointer_type LL.ctx
end

module BindingCtx = struct
  type def = Local of Bind.t | Top of Bind.t

  type t =
    | BindMap of {
        locals : (string, Bind.t, String.comparator_witness) Map.t;
        toplevels : (string, Bind.t, String.comparator_witness) Map.t;
        accessors : (string, Bind.t, String.comparator_witness) Map.t;
      }

  exception UnboundedName of string

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

  let toplevel (BindMap { toplevels; _ }) name =
    match Map.find toplevels name with
    | Some v -> v
    | None -> raise (UnboundedName name)

  let accessor (BindMap { accessors; _ }) name =
    match Map.find accessors name with
    | Some v -> v
    | None -> raise (UnboundedName name)

  let add name bind (BindMap bm) =
    BindMap { bm with locals = Map.set bm.locals ~key:name ~data:bind }

  let append_locals (BindMap bm) new_locals =
    BindMap { bm with locals = Map.merge_disjoint_exn bm.locals new_locals }

  let replace_locals (BindMap { accessors; toplevels; _ }) l =
    BindMap
      { locals = Map.of_alist_exn (module String) l; toplevels; accessors }

  let of_maps ~accessors ~toplevels ~locals =
    BindMap { locals; toplevels; accessors }

  let of_lists ~accessors ~toplevels ~locals =
    BindMap
      {
        locals = Map.of_alist_exn (module String) locals;
        toplevels = Map.of_alist_exn (module String) toplevels;
        accessors = Map.of_alist_exn (module String) accessors;
      }

  let empty =
    BindMap
      {
        locals = Map.empty (module String);
        toplevels = Map.empty (module String);
        accessors = Map.empty (module String);
      }
end

module TagsCtx (LL : LowLevelCtx) = struct
  type t =
    | TagsCtx of {
        checkers : (string, Bind.t, String.comparator_witness) Map.t;
        tag_types : (string, lltype, String.comparator_witness) Map.t;
      }

  let of_lists c tt =
    TagsCtx
      {
        checkers = Map.of_alist_exn (module String) c;
        tag_types = Map.of_alist_exn (module String) tt;
      }

  let checker_tpe = function_type (i1_type LL.ctx) [| pointer_type LL.ctx |]

  let tag_type (TagsCtx self) constructor_name =
    Map.find_exn self.tag_types constructor_name

  let tag_checker (TagsCtx self) name =
    Bind.single_exn (Map.find_exn self.checkers name)
end

module ActorContext = struct
  type state_info = { layout : lltype; fields : (string * lltype) list }

  type t =
    | LayoutCtx of {
        state_layouts : (string, state_info, String.comparator_witness) Map.t;
        mutators : (string, llvalue, String.comparator_witness) Map.t;
      }

  let state_layout (LayoutCtx self) state =
    Map.find_exn self.state_layouts state

  let mutator (LayoutCtx self) actor = Map.find_exn self.mutators actor

  let of_lists state_layouts mutators =
    LayoutCtx
      {
        state_layouts = Map.of_alist_exn (module String) state_layouts;
        mutators = Map.of_alist_exn (module String) mutators;
      }

  let state layout fields = { layout; fields }
end

module CompilerCtx (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)

  type t = {
    tag_ctx : TagMap.t;
    bind_ctx : BindingCtx.t;
    type_ctx : TypeMap.t;
    actor_ctx : ActorContext.t;
    runtime : Runtime.t;
    mutable builder : llbuilder;
  }

  let init ~tags ~binds ~types ~actors ~runtime ~builder =
    {
      tag_ctx = tags;
      bind_ctx = binds;
      type_ctx = types;
      actor_ctx = actors;
      runtime;
      builder;
    }
end

module TopLevelCtx (LL : LowLevelCtx) = struct
  module TypeMap = PolyCtx (LL)
  module TagMap = TagsCtx (LL)
  module LocalContext = CompilerCtx (LL)

  type t = {
    tag_ctx : TagMap.t;
    bind_ctx : BindingCtx.t;
    actor_ctx : ActorContext.t;
    runtime : Runtime.t;
  }

  let to_local self locals builder =
    LocalContext.init ~tags:self.tag_ctx
      ~binds:(BindingCtx.replace_locals self.bind_ctx locals)
      ~types:TypeMap.empty ~actors:self.actor_ctx ~runtime:self.runtime ~builder

  let to_local_with_types self locals types builder =
    LocalContext.init ~tags:self.tag_ctx
      ~binds:(BindingCtx.replace_locals self.bind_ctx locals)
      ~types ~actors:self.actor_ctx ~runtime:self.runtime ~builder
end
