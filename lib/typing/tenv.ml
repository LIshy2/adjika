open Core

module Gen = struct
  type type_id = int

  let tid_counter = ref 0

  let new_var () =
    let result = !tid_counter in
    tid_counter := result + 1;
    Type.TypeVar result

  let rec replace map = function
    | Type.Int -> Type.Int
    | Type.TypeVar id ->
        if Map.mem map id then Map.find_exn map id else Type.TypeVar id
    | Type.Arrow (args, result) ->
        Type.Arrow (List.map args ~f:(replace map), replace map result)
    | Type.Named name -> Type.Named name
    | Type.Operator (name, arguments) ->
        Type.Operator (name, List.map arguments ~f:(replace map))
    | t -> t

  let instantiate = function
    | Type.Mono m -> m
    | Type.Quant (ids, mono) ->
        let replace_map =
          Map.of_alist_exn
            (module Int)
            (List.map ids ~f:(fun id -> (id, new_var ())))
        in
        replace replace_map mono
end

module Constructor = struct
  type poly = { result : Type.poly; fields : Type.mono list }
  type mono = { instance : Type.mono; fields : Type.mono list }

  let poly result fields = { result; fields }
  let mono instance fields = { instance; fields }

  let instantiate { result; fields; _ } =
    match result with
    | Type.Mono m -> { instance = m; fields }
    | Type.Quant (ids, mono) ->
        let replace_map =
          Map.of_alist_exn
            (module Int)
            (List.map ids ~f:(fun id -> (id, Gen.new_var ())))
        in
        let result = Gen.replace replace_map mono in
        {
          instance = result;
          fields = List.map fields ~f:(Gen.replace replace_map);
        }

  let instance constructor = constructor.instance
  let fields constructor = constructor.fields
end

type t = {
  local_env : (string, Type.poly, String.comparator_witness) Map.t;
  fields_env : (string, Type.poly, String.comparator_witness) Map.t;
  constructors : (string, Constructor.poly, String.comparator_witness) Map.t;
}

exception UnknownName of string

let find { local_env; _ } name =
  match Map.find local_env name with
  | Some v -> v
  | None -> raise (UnknownName name)

let find_field { fields_env; _ } name =
  match Map.find fields_env name with
  | Some v -> v
  | None -> raise (UnknownName name)

let add name id self =
  (* print_endline name; *)
  (* print_endline (Sexp.to_string (Map.sexp_of_m__t (module String) (Type.sexp_of_poly) self.local_env)); *)
  { self with local_env = Map.add_exn self.local_env ~key:name ~data:id }

let empty =
  {
    local_env = Map.empty (module String);
    fields_env = Map.empty (module String);
    constructors = Map.empty (module String);
  }

let of_list ~locals ~fields ~constructors =
  {
    local_env = Map.of_alist_exn (module String) locals;
    fields_env = Map.of_alist_exn (module String) fields;
    constructors = Map.of_alist_exn (module String) constructors;
  }

let generealize { local_env; _ } typ =
  let context_vars =
    Set.union_list
      (module Int)
      (List.map (Map.to_alist local_env) ~f:(fun (_, v) -> Type.freevars v))
  in
  let quants = Set.diff (Type.freevars_mono typ) context_vars in
  if Set.length quants > 0 then Type.Quant (Set.to_list quants, typ)
  else Type.Mono typ

let constructor self name =
  match Map.find self.constructors name with
  | Some v -> v
  | None -> raise (UnknownName name)

let apply_to_locals f self = { self with local_env = Map.map ~f self.local_env }

let apply_to_fields f self =
  { self with fields_env = Map.map ~f self.fields_env }
