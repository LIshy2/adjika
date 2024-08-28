open Core

module TypeDecl = struct
  type t = Arrow of t list * t | Int
  [@@deriving sexp, compare]

  let arrow args res = Arrow (args, res)
  let int = Int
end

module Deconstructor = struct
  type t = Var of string | Constructor of string * t list
  [@@deriving sexp, compare]

  let var name = Var name
  let constructor name args = Constructor (name, args)
end

module Symbol = struct
  type 'e fun_symbol = { name : string; arguments : string list; result : 'e }
  [@@deriving sexp, compare]
  type 'e val_symbol = { name : string; result : 'e }
  [@@deriving sexp, compare]
  type 't con_symbol = { name : string; fields : (string * 't) list }
  [@@deriving sexp, compare]
  type 't type_symbol = { name : string; constructors : 't con_symbol list }
  [@@deriving sexp, compare]

  let field_names con = List.map con.fields ~f:(fun (name, _) -> name)
  let field_types con = List.map con.fields ~f:(fun (_, tpe) -> tpe)
  let constructors_names ts = List.map ts.constructors ~f:(fun c -> c.name)
end

module BinOp = struct
  type t = Plus | Minus | Mult
  [@@deriving sexp, compare]
end

module Expr = struct
  type expr =
    | Var of string
    | Const of int
    | Oper of BinOp.t * expr * expr
    | Block of expr Symbol.val_symbol list * expr
    | Field of expr * string
    | Apply of expr * expr list
    | Lambda of string list * expr
    | PatMatch of expr * (Deconstructor.t * expr) list
  [@@deriving sexp, compare]

  let var name = Var name
  let const value = Const value
  let plus lhs rhs = Oper (BinOp.Plus, lhs, rhs)
  let minus lhs rhs = Oper (BinOp.Minus, lhs, rhs)
  let mult lhs rhs = Oper (BinOp.Mult, lhs, rhs)
  let block defs result = Block (defs, result)
  let apply name arguments = Apply (name, arguments)
  let field str name = Field (str, name)
  let lambda arguments result = Lambda (arguments, result)
  let pat_match obj cases = PatMatch (obj, cases)
end

module Toplevel = struct
  type ('e, 't) decl =
    | FunExpression of 'e Symbol.fun_symbol
    | TypeDefenition of 't Symbol.type_symbol
  [@@deriving sexp, compare]

  let fundecl name arguments result = FunExpression { name; arguments; result }
  let valdecl name result = Symbol.{ name; result }
  let typedecl name constructors = TypeDefenition Symbol.{ name; constructors }
  let condecl name fields = Symbol.{ name; fields }
end

module Program = struct
  type ('e, 't) t = {
    functions : 'e Symbol.fun_symbol list;
    types : 't Symbol.type_symbol list;
  } [@@deriving sexp, compare]

  let from_toplevels toplevels =
    let functions, types =
      List.partition_map toplevels ~f:(fun d ->
          match d with
          | Toplevel.FunExpression f -> Either.First f
          | Toplevel.TypeDefenition t -> Either.Second t)
    in
    { functions; types }
end


