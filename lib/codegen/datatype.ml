open Core
open Llvm
open Context
open Typing
open Parsing.Ast.Datatype

let constructor_type con result =
  let args = List.map con.fields ~f:(fun (_, tpe) -> tpe) in
  Type.Arrow (args, result)

module RecordCompiler (LL : LowLevelCtx) = struct
  module PolyMap = PolyCtx (LL)

  type record = {
    constructor : string * Bind.t;
    accessors : (string * Bind.t) list;
  }

  let compile_constructor tm constructor =
    let fields =
      List.map constructor.fields ~f:(fun (_, tpe) ->
          PolyMap.lltype_elem tm tpe)
    in
    let layout = struct_type LL.ctx (Array.of_list fields) in
    let constructor_ty =
      function_type (pointer_type LL.ctx)
        (Array.of_list (pointer_type LL.ctx :: fields))
    in
    let f = define_function constructor.name constructor_ty LL.md in
    let bd = builder_at_end LL.ctx (entry_block f) in
    let instance = build_malloc layout "instance" bd in
    let _ =
      Array.iteri (params f) ~f:(fun i field_value ->
          if i > 0 then
            let field_ptr =
              build_struct_gep layout instance (i - 1) "field.ptr" bd
            in
            let _ = build_store field_value field_ptr bd in
            ()
          else ())
    in
    let _ = build_ret instance bd in
    f

  let compile_acessor layout name ind =
    let llt = (struct_element_types layout).(ind) in
    let accessor_ty = function_type llt [| pointer_type LL.ctx |] in
    let f = define_function (name ^ ".get") accessor_ty LL.md in
    let bd = builder_at_end LL.ctx (entry_block f) in
    let instance = (params f).(0) in
    let field_ptr = build_struct_gep layout instance ind (name ^ ".ptr") bd in
    let field_value = build_load llt field_ptr name bd in
    let _ = build_ret field_value bd in
    f

  let compile_record constructor =
    let fields =
      Array.of_list
        (List.map constructor.fields ~f:(fun (_, tpe) ->
             PolyMap.lltype_elem PolyMap.empty tpe))
    in
    let layout = struct_type LL.ctx fields in
    let accessors =
      List.mapi constructor.fields ~f:(fun i (name, _) ->
          (name, Bind.Single (compile_acessor layout name i)))
    in
    let compiled_constructor = compile_constructor PolyMap.empty constructor in
    {
      constructor = (constructor.name, Bind.Single compiled_constructor);
      accessors;
    }

  let compile_poly_record quants constructor result =
    let specs = PolyMap.variants PolyMap.empty quants in
    let compiled_constructor =
      Bind.Versions
        (List.map specs ~f:(fun tm ->
             let v = compile_constructor tm constructor in
             (PolyMap.type_to_bind tm (constructor_type constructor result), v)))
    in

    let accessors =
      List.mapi constructor.fields ~f:(fun i (name, field_type) ->
          ( name,
            Bind.Versions
              (List.map specs ~f:(fun tm ->
                   let fields =
                     List.map constructor.fields ~f:(fun (_, tpe) ->
                         PolyMap.lltype_elem tm tpe)
                   in
                   let layout = struct_type LL.ctx (Array.of_list fields) in
                   let v = compile_acessor layout name i in
                   let accessor_type = Type.Arrow ([ result ], field_type) in
                   (PolyMap.type_to_bind tm accessor_type, v))) ))
    in
    { constructor = (constructor.name, compiled_constructor); accessors }
end

module SumCompiler (LL : LowLevelCtx) = struct
  module PolyMap = PolyCtx (LL)

  type sum = {
    checkers : (string * Bind.t) list;
    constructors : (string * Bind.t) list;
    tags : (string * lltype) list;
  }

  let tag_type constructors =
    let count = List.length constructors in
    if count <= 2 then i1_type LL.ctx
    else if count <= 256 then i8_type LL.ctx
    else if count <= 65536 then i16_type LL.ctx
    else i32_type LL.ctx

  let tag_types constructors =
    let tt = tag_type constructors in
    List.map constructors ~f:(fun t -> (t.name, tt))

  let compile_checkers constructors =
    let tag_type = tag_type constructors in
    List.mapi constructors ~f:(fun ind con ->
        let checker_type =
          function_type (i1_type LL.ctx) [| pointer_type LL.ctx |]
        in
        let checker_fun =
          define_function (con.name ^ ".checker") checker_type LL.md
        in
        let builder = builder_at_end LL.ctx (entry_block checker_fun) in
        let instance = (params checker_fun).(0) in
        let tag_ptr =
          build_struct_gep
            (struct_type LL.ctx [| tag_type |])
            instance 0 "tag.ptr" builder
        in
        let tag = build_load tag_type tag_ptr "tag.val" builder in
        let cmp =
          build_icmp Icmp.Eq tag (const_int tag_type ind) "result" builder
        in
        let _ = build_ret cmp builder in
        (con.name, Bind.Single checker_fun))

  let compile_constructor tm tt ind con =
    let fields =
      List.map con.fields ~f:(fun (_, t) -> PolyMap.lltype_elem tm t)
    in
    let layout = struct_type LL.ctx (Array.of_list (tt :: fields)) in
    let constructor_type =
      function_type (pointer_type LL.ctx)
        (Array.of_list (pointer_type LL.ctx :: fields))
    in
    let constructor_fun = define_function con.name constructor_type LL.md in
    let bd = builder_at_end LL.ctx (entry_block constructor_fun) in
    let instance = build_malloc layout "instance" bd in
    let tag_ptr =
      build_struct_gep (struct_type LL.ctx [| tt |]) instance 0 "tag_ptr" bd
    in
    let _ = build_store (const_int tt ind) tag_ptr bd in

    let _ =
      Array.iteri (params constructor_fun) ~f:(fun i field_value ->
          if i > 0 then
            let field_ptr = build_struct_gep layout instance i "field_ptr" bd in
            let _ = build_store field_value field_ptr bd in
            ()
          else ())
    in
    let _ = build_ret instance bd in
    constructor_fun

  let compile_sum constructors =
    let tags = tag_types constructors in
    let checkers = compile_checkers constructors in
    let tag_type = tag_type constructors in
    let constructors =
      List.mapi constructors ~f:(fun ind con ->
          let v = compile_constructor PolyMap.empty tag_type ind con in
          (con.name, Bind.Single v))
    in
    { checkers; constructors; tags }

  let compile_poly_sum quants constructors result =
    let tags = tag_types constructors in
    let checkers = compile_checkers constructors in
    let specs = PolyMap.variants PolyMap.empty quants in
    let tag_type = tag_type constructors in
    let constructors =
      List.mapi constructors ~f:(fun ind con ->
          let s =
            List.map specs ~f:(fun tm ->
                let v = compile_constructor tm tag_type ind con in
                (PolyMap.type_to_bind tm (constructor_type con result), v))
          in
          (con.name, Bind.Versions s))
    in
    { checkers; constructors; tags }
end

module TypeCompiler (LL : LowLevelCtx) = struct
  module PolyMap = PolyCtx (LL)
  module RecordCompiler = RecordCompiler (LL)
  module SumCompiler = SumCompiler (LL)

  type compiled_type =
    | Record of RecordCompiler.record
    | Sum of SumCompiler.sum

  let compile_type tpe =
    match tpe with
    | Mono { constructors; _ } -> (
        match constructors with
        | [ single ] -> Record (RecordCompiler.compile_record single)
        | multi -> Sum (SumCompiler.compile_sum multi))
    | Operator { name; constructors; arguments; _ } -> (
        let type_arguments =
          List.map arguments ~f:(fun id -> Type.TypeVar id)
        in
        match constructors with
        | [ single ] ->
            Record
              (RecordCompiler.compile_poly_record arguments single
                 (Type.Operator (name, type_arguments)))
        | multi ->
            Sum
              (SumCompiler.compile_poly_sum arguments multi
                 (Type.Operator (name, type_arguments))))
end
