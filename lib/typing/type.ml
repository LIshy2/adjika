open Core

type mono =
  | TypeVar of int
  | Int
  | Arrow of mono list * mono
  | Named of string
  | Operator of string * mono list
  | MailBox of string
  | Actor of string
[@@deriving compare, sexp]

type poly = Mono of mono | Quant of int list * mono [@@deriving sexp]

let compare_poly lhs rhs =
  match (lhs, rhs) with
  | Mono l, Mono r -> compare_mono l r
  | Quant (lid, lm), Quant (rid, rm) ->
      let quant_id = Map.of_alist_exn (module Int) (List.zip_exn lid rid) in
      let rec replace_left = function
        | TypeVar id ->
            TypeVar (Option.value ~default:id (Map.find quant_id id))
        | Int -> Int
        | Arrow (args, result) ->
            Arrow (List.map ~f:replace_left args, replace_left result)
        | Named name -> Named name
        | Operator (name, args) -> Operator (name, List.map ~f:replace_left args)
        | MailBox mail -> MailBox mail
        | Actor name -> Actor name
      in
      compare_mono (replace_left lm) rm
  | Mono _, Quant _ -> -1
  | Quant _, Mono _ -> 1

let rec show_mono = function
  | Arrow (args, res) ->
      let args = String.concat ~sep:", " (List.map args ~f:show_mono) in
      "(" ^ args ^ ") -> " ^ show_mono res
  | Int -> "int"
  | Named name -> name
  | Operator (name, args) ->
      name ^ "[" ^ String.concat ~sep:", " (List.map ~f:show_mono args) ^ "]"
  | TypeVar id -> string_of_int id
  | MailBox actor -> "Mailbox[" ^ actor ^ "]"
  | Actor name -> "Actor[" ^ name ^ "]"

let show_poly = function
  | Mono mono -> show_mono mono
  | Quant (quants, m) ->
      String.concat ~sep:" "
        (List.map quants ~f:(fun q -> "∀" ^ string_of_int q ^ "."))
      ^ " " ^ show_mono m

let rec freevars_mono = function
  | Operator (_, args) ->
      Set.union_list (module Int) (List.map args ~f:freevars_mono)
  | TypeVar id -> Set.singleton (module Int) id
  | Arrow (args, result) ->
      Set.union (freevars_mono result)
        (Set.union_list (module Int) (List.map args ~f:freevars_mono))
  | _ -> Set.empty (module Int)

let freevars = function
  | Mono m -> freevars_mono m
  | Quant (ids, m) -> Set.diff (freevars_mono m) (Set.of_list (module Int) ids)

let monotype = function Mono mono -> mono | Quant (_, mono) -> mono

let rec from_decl = function
  | Parsing.Ast.TypeDecl.Arrow (args, result) ->
      Arrow (List.map ~f:from_decl args, from_decl result)
  | Parsing.Ast.TypeDecl.Int -> Int
  | Parsing.Ast.TypeDecl.Custom name -> Named name
  | Parsing.Ast.TypeDecl.Operator (name, args) ->
      Operator (name, List.map ~f:from_decl args)
  | MailBox actor -> MailBox actor

let rec from_decl_ctx ctx = function
  | Parsing.Ast.TypeDecl.Arrow (args, result) ->
      Arrow (List.map ~f:(from_decl_ctx ctx) args, from_decl_ctx ctx result)
  | Parsing.Ast.TypeDecl.Int -> Int
  | Parsing.Ast.TypeDecl.Custom name -> (
      match Map.find ctx name with None -> Named name | Some t -> t)
  | Parsing.Ast.TypeDecl.Operator (name, args) ->
      Operator (name, List.map ~f:(from_decl_ctx ctx) args)
  | Parsing.Ast.TypeDecl.MailBox name -> MailBox name
