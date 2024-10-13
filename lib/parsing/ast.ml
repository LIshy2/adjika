open Core

module TypeDecl = struct
  type t =
    | Arrow of t list * t
    | Int
    | Float
    | Bool
    | Custom of string
    | Operator of string * t list
    | MailBox of string
  [@@deriving sexp, compare]

  let arrow args res = Arrow (args, res)
  let int = Int
  let custom name = Custom name
  let operator name arguments = Operator (name, arguments)
  let mailbox actor = MailBox actor
end

module Deconstructor = struct
  type t = Var of string | Constructor of string * t list
  [@@deriving sexp, compare]

  let var name = Var name
  let constructor name args = Constructor (name, args)
end

module Actor = struct
  type 't state = { name : string; fields : (string * 't) list }
  [@@deriving sexp, compare]

  type 't t = { name : string; states : 't state list }
  [@@deriving sexp, compare]

  let state name fields = { name; fields }
  let decl name states = { name; states }
end

module Val = struct
  type 'e t = { name : string; result : 'e } [@@deriving sexp, compare]

  let decl name result = { name; result }
end

module Handler = struct
  type ('v, 'e) statement =
    | Spawn of { name : string; actor : 'e }
    | Val of 'v
    | Mutate of 'e
    | Send of { message : 'e; mail : 'e }
  [@@deriving sexp, compare]

  type ('e, 'v, 't) t = {
    message_type : 't;
    state : string;
    body : ('v, 'e) statement list;
  }
  [@@deriving sexp, compare]

  let spawn name actor = Spawn { name; actor }
  let local name result = Val Val.{ name; result }
  let mutate build = Mutate build
  let send message mail = Send { message; mail }
  let decl message_type state body = { message_type; state; body }
end

module Function = struct
  type 'e t = { name : string; arguments : string list; result : 'e }
  [@@deriving sexp, compare]

  let decl name arguments result = { name; arguments; result }
end

module Datatype = struct
  type 't constructor = { name : string; fields : (string * 't) list }
  [@@deriving sexp, compare]

  type ('t, 'a) t =
    | Mono of { name : string; constructors : 't constructor list }
    | Operator of {
        name : string;
        arguments : 'a list;
        constructors : 't constructor list;
      }
  [@@deriving sexp, compare]

  let constructor name fields = { name; fields }
  let mono name constructors = Mono { name; constructors }

  let operator name arguments constructors =
    Operator { name; arguments; constructors }

  let field_names con = List.map con.fields ~f:(fun (name, _) -> name)
  let field_types con = List.map con.fields ~f:(fun (_, tpe) -> tpe)

  let constructors ts =
    match ts with
    | Mono { constructors; _ } -> constructors
    | Operator { constructors; _ } -> constructors

  let constructors_names ts = List.map (constructors ts) ~f:(fun c -> c.name)
end

module BinOp = struct
  type t = Plus | Minus | Mult [@@deriving sexp, compare]
end

module Expr = struct
  type t =
    | Var of string
    | Const of int
    | Oper of BinOp.t * t * t
    | Block of t Val.t list * t
    | Field of t * string
    | Apply of t * t list
    | Lambda of string list * t
    | PatMatch of t * (Deconstructor.t * t) list
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
  type ('e, 'v, 't) decl =
    | FunExpression of 'e Function.t
    | TypeDefenition of ('t, string) Datatype.t
    | ActorDefition of 't Actor.t
    | HandlerDefinition of ('e, 'v, 't) Handler.t
  [@@deriving sexp, compare]
end

module Program = struct
  type ('e, 'v, 't) t = {
    functions : 'e Function.t list;
    types : ('t, string) Datatype.t list;
    actors : 't Actor.t list;
    handlers : ('e, 'v, 't) Handler.t list;
  }
  [@@deriving sexp, compare]

  let from_toplevels toplevels =
    let functions, types, actors, handlers =
      List.fold_right toplevels ~init:([], [], [], [])
        ~f:(fun tl (f, t, a, h) ->
          match tl with
          | Toplevel.FunExpression fe -> (fe :: f, t, a, h)
          | Toplevel.TypeDefenition td -> (f, td :: t, a, h)
          | Toplevel.ActorDefition ad -> (f, t, ad :: a, h)
          | Toplevel.HandlerDefinition hd -> (f, t, a, hd :: h))
    in
    { functions; types; actors; handlers }
end
