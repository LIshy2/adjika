module TypeDecl : sig
  type t = Arrow of t list * t | Int [@@deriving sexp, compare]

  val arrow : t list -> t -> t
  val int : t
end

module Deconstructor : sig
  type t = Var of string | Constructor of string * t list [@@deriving sexp, compare]

  val var : string -> t
  val constructor : string -> t list -> t
end

module Symbol : sig
  type 'e fun_symbol = { name : string; arguments : string list; result : 'e }
  [@@deriving sexp, compare]

  type 'e val_symbol = { name : string; result : 'e } [@@deriving sexp, compare]

  type 't con_symbol = { name : string; fields : (string * 't) list }
  [@@deriving sexp, compare]

  type 't type_symbol = { name : string; constructors : 't con_symbol list }
  [@@deriving sexp, compare]

  val field_names : 'a con_symbol -> string list
  val field_types : 'a con_symbol -> 'a list
  val constructors_names : 'a type_symbol -> string list
end

module BinOp : sig
  type t = Plus | Minus | Mult
end

module Expr : sig
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

  val var : string -> expr
  val const : int -> expr
  val plus : expr -> expr -> expr
  val minus : expr -> expr -> expr
  val mult : expr -> expr -> expr
  val block : expr Symbol.val_symbol list -> expr -> expr
  val apply : expr -> expr list -> expr
  val field : expr -> string -> expr
  val lambda : string list -> expr -> expr
  val pat_match : expr -> (Deconstructor.t * expr) list -> expr
end

module Toplevel : sig
  type ('e, 't) decl =
    | FunExpression of 'e Symbol.fun_symbol
    | TypeDefenition of 't Symbol.type_symbol
  [@@deriving sexp, compare]

  val fundecl : string -> string list -> 'a -> ('a, 'b) decl
  val valdecl : string -> 'a -> 'a Symbol.val_symbol
  val typedecl : string -> 'a Symbol.con_symbol list -> ('b, 'a) decl
  val condecl : string -> (string * 'a) list -> 'a Symbol.con_symbol
end

module Program : sig
  type ('e, 't) t = {
    functions : 'e Symbol.fun_symbol list;
    types : 't Symbol.type_symbol list;
  }
  [@@deriving sexp, compare]

  val from_toplevels : ('a, 'b) Toplevel.decl list -> ('a, 'b) t
end
