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

type poly = Mono of mono | Quant of int list * mono [@@deriving compare, sexp]

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
  | TypeVar id ->
      Set.singleton (module Int) id
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
