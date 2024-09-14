open Core
open Parsing

module DSU = struct
  type t = { parent : (int, Type.mono) Hashtbl.t }

  let create () = { parent = Hashtbl.create (module Int) }

  let rec get_ind self i =
    match Hashtbl.find self.parent i with
    | None -> Type.TypeVar i
    | Some (TypeVar f) ->
        if not (f = i) then (
          let result = get_ind self f in
          Hashtbl.set self.parent ~key:f ~data:result;
          result)
        else TypeVar f
    | Some t -> t

  let rec apply self tpe =
    match tpe with
    | Type.TypeVar ind -> (
        match get_ind self ind with
        | Type.TypeVar new_ind ->
            if new_ind = ind then Type.TypeVar ind
            else apply self (Type.TypeVar new_ind)
        | t -> apply self t)
    | Type.Arrow (args, result) ->
        Type.Arrow (List.map args ~f:(apply self), apply self result)
    | Type.Operator (name, args) ->
        Type.Operator (name, List.map args ~f:(apply self))
    | t -> t

  exception NoUnion

  let union self i j =
    let pi = apply self i in
    let pj = apply self j in
    match (pi, pj) with
    | Type.TypeVar li, Type.TypeVar ri ->
        if li = ri then ()
        else if li <= ri then
          Hashtbl.set ~key:ri ~data:(Type.TypeVar li) self.parent
        else Hashtbl.set ~key:li ~data:(Type.TypeVar ri) self.parent
    | Type.TypeVar id, other ->
        Hashtbl.set ~key:id ~data:other self.parent
    | other, Type.TypeVar id -> Hashtbl.set ~key:id ~data:other self.parent
    | _ -> raise NoUnion
end

type 'res t = DSU.t -> 'res

module Let_syntax = struct
  module Let_syntax = struct
    let return x _ = x
    let bind self ~f dsu = f (self dsu) dsu
    let map self ~f dsu = f (self dsu)
    let both l r dsu = (l dsu, r dsu)

    module Open_on_rhs = struct end
  end

  let return = Let_syntax.return
  let bind = Let_syntax.bind
  let map = Let_syntax.map
  let both = Let_syntax.both
end

let compose_list list ~f =
  let open Let_syntax in
  List.fold_right list ~init:(return []) ~f:(fun x acc ->
      let%bind others = acc in
      let%map fx = f x in
      fx :: others)

let run_substitution exec = exec (DSU.create ())
let apply t dsu = DSU.apply dsu t

let apply_poly = function
  | Type.Mono mono ->
      let%map mt = apply mono in
      Type.Mono mt
  | Type.Quant (ids, mono) ->
      let%map mt = apply mono in
      Type.Quant (ids, mt)

let rec apply_ast ast dsu =
  match ast with
  | Texp.Var (name, t) -> Texp.Var (name, apply_poly t dsu)
  | Texp.Const (value, t) -> Texp.Const (value, apply_poly t dsu)
  | Texp.Oper (binop, lhs, rhs, t) ->
      Texp.Oper (binop, apply_ast lhs dsu, apply_ast rhs dsu, apply_poly t dsu)
  | Texp.Lambda (arguments, body, t) ->
      Texp.Lambda (arguments, apply_ast body dsu, apply_poly t dsu)
  | Texp.Field (obj, name, t) ->
      Texp.Field (apply_ast obj dsu, name, apply_poly t dsu)
  | Texp.Block (vals, result) ->
      let apply_val Ast.Val.{ name; result } =
        Ast.Val.{ name; result = apply_ast result dsu }
      in
      Texp.Block (List.map vals ~f:apply_val, apply_ast result dsu)
  | Texp.PatMatch (obj, cases, t) ->
      let rec apply_dec = function
        | Texp.Deconstructor.Constructor (name, subs) ->
            Texp.Deconstructor.Constructor
              (name, List.map subs ~f:(fun (d, t) -> (apply_dec d, apply t dsu)))
        | var -> var
      in
      Texp.PatMatch
        ( apply_ast obj dsu,
          List.map cases ~f:(fun (d, e) -> (apply_dec d, apply_ast e dsu)),
          apply_poly t dsu )
  | Texp.Apply (f, args, t) ->
      Texp.Apply
        ( apply_ast f dsu,
          List.map args ~f:(fun a -> apply_ast a dsu),
          apply_poly t dsu )

exception Uninifiable of (Type.mono * Type.mono)

let rec unify type_1 type_2 =
  let open Let_syntax in
  let%bind type_1 = apply type_1 in
  let%bind type_2 = apply type_2 in
  match (type_1, type_2) with
  | Type.Int, Type.Int -> return ()
  | Type.Named name1, Type.Named name2 ->
      if String.equal name1 name2 then return ()
      else raise (Uninifiable (type_1, type_2))
  | Type.Arrow (type_args1, result_type1), Arrow (type_args2, result_type2) ->
      let%bind _ =
        compose_list (List.zip_exn type_args1 type_args2)
          ~f:(fun (arg1, arg2) ->
            let%bind a1 = apply arg1 in
            let%bind a2 = apply arg2 in
            unify a1 a2)
      in
      let%bind r1 = apply result_type1 in
      let%bind r2 = apply result_type2 in
      let%map _ = unify r1 r2 in
      ()
  | Type.Operator (name1, args1), Type.Operator (name2, args2) ->
      if String.equal name1 name2 then
        let%map _ =
          compose_list (List.zip_exn args1 args2) ~f:(fun (arg1, arg2) ->
              let%bind a1 = apply arg1 in
              let%bind a2 = apply arg2 in
              unify a1 a2)
        in
        ()
      else raise (Uninifiable (type_1, type_2))
  | Type.MailBox name1, Type.MailBox name2 ->
      if
        String.equal name1 name2 || String.equal name1 "?"
        || String.equal name2 "?"
      then return ()
      else raise (Uninifiable (type_1, type_2))
  | Type.Actor name1, Type.Actor name2 ->
      if
        String.equal name1 name2 || String.equal name1 "?"
        || String.equal name2 "?"
      then return ()
      else raise (Uninifiable (type_1, type_2))
  | lhs, rhs -> (
      fun dsu ->
        try DSU.union dsu lhs rhs
        with DSU.NoUnion -> raise (Uninifiable (type_1, type_2)))
