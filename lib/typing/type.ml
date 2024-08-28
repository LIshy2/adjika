open Core

type mono =
  | TypeVar of int
  | Int
  | Arrow of mono list * mono
  | Custom of string

type poly = Mono of mono | Quant of int list * mono

let rec show_mono = function
  | Arrow (args, res) ->
      let args = String.concat ~sep:", " (List.map args ~f:show_mono) in
      "(" ^ args ^ ") -> " ^ show_mono res
  | Int -> "int"
  | Custom name -> name
  | TypeVar id -> string_of_int id

let show_poly = function
  | Mono mono -> show_mono mono
  | Quant (quants, m) ->
      String.concat ~sep:" "
        (List.map quants ~f:(fun q -> "∀" ^ string_of_int q ^ "."))
      ^ " " ^ show_mono m

let rec freevars_mono = function
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
