open Core


module Gen = struct 
  type type_id = int

  let tid_counter = ref 0

  let new_var () =
    let result = !tid_counter in
    tid_counter := result + 1;
    Type.TypeVar result

  let instantiate = function
    | Type.Mono m -> m
    | Type.Quant (ids, mono) ->
        let replace_map =
          Map.of_alist_exn
            (module Int)
            (List.map ids ~f:(fun id -> (id, new_var ())))
        in
        let rec replace = function
          | Type.Int -> Type.Int
          | Type.TypeVar id ->
              if List.mem ids id ~equal:(fun x y -> phys_equal x y) then
                Map.find_exn replace_map id
              else Type.TypeVar id
          | Type.Arrow (args, result) ->
              Type.Arrow (List.map args ~f:replace, replace result)
          | custom -> custom
        in
        replace mono
end 

type tcon = {
  result: Type.mono;
  constructo_type: Type.poly;
  fields: Type.mono list;
}

type t = {
  local_env : (string, Type.poly, String.comparator_witness) Map.t;
  fields_env : (string, Type.poly, String.comparator_witness) Map.t;
  constructors: (string, tcon, String.comparator_witness) Map.t;
}

let find { local_env; _ } name =
  match Map.find local_env name with
  | Some v -> v
  | None ->
      print_endline ("Fatal error: unknown name " ^ name ^ " in typechecking");
      exit (-1)

let add self name id =
  { self with local_env = Map.add_exn self.local_env ~key:name ~data:id }

let empty =
  {
    local_env = Map.empty (module String);
    fields_env = Map.empty (module String);
    constructors = Map.empty (module String);
  }

let of_list ~locals ~fields =
  {
    local_env = Map.of_alist_exn (module String) locals;
    fields_env = Map.of_alist_exn (module String) fields;
    constructors = Map.empty (module String);
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
  Map.find_exn self.constructors name 
