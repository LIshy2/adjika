module TypeDecl : sig
  type t =
    | Arrow of t list * t
    | Int
    | Float
    | Bool
    | Custom of string
    | Operator of string * t list
    | MailBox of string
  [@@deriving sexp, compare]

  val arrow : t list -> t -> t
  val int : t
  val custom : string -> t
  val operator : string -> t list -> t
  val mailbox : string -> t
end

module Deconstructor : sig
  type t = Var of string | Constructor of string * t list
  [@@deriving sexp, compare]

  val var : string -> t
  val constructor : string -> t list -> t
end

module Actor : sig
  type 't state = { name : string; fields : (string * 't) list }
  [@@deriving sexp, compare]

  type 't t = { name : string; states : 't state list }
  [@@deriving sexp, compare]

  val state : string -> (string * 't) list -> 't state
  val decl : string -> 't state list -> 't t
end

module Handler : sig
  type 'e statement =
    | Spawn of { name : string; actor : 'e }
    | Val of { name : string; result : 'e }
    | Mutate of 'e
    | Send of { message : 'e; mail : 'e }
  [@@deriving sexp, compare]

  type ('e, 't) t = {
    message_type : 't;
    state : string;
    body : 'e statement list;
  }
  [@@deriving sexp, compare]

  val spawn : string -> 'e -> 'e statement
  val local : string -> 'e -> 'e statement
  val mutate : 'e -> 'e statement
  val send : 'e -> 'e -> 'e statement
  val decl : 't -> string -> 'e statement list -> ('e, 't) t
end

module Function : sig
  type 'e t = { name : string; arguments : string list; result : 'e }
  [@@deriving sexp, compare]

  val decl : string -> string list -> 'e -> 'e t
end

module Val : sig
  type 'e t = { name : string; result : 'e } [@@deriving sexp, compare]

  val decl : string -> 'e -> 'e t
end

module Datatype : sig
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

  val constructor : string -> (string * 't) list -> 't constructor
  val mono : string -> 't constructor list -> ('t, 'a) t
  val operator : string -> 'a list -> 't constructor list -> ('t, 'a) t
  val field_names : 't constructor -> string list
  val field_types : 't constructor -> 't list
  val constructors : ('t, 'a) t -> 't constructor list
  val constructors_names : ('t, 'a) t -> string list
end

module BinOp : sig
  type t = Plus | Minus | Mult [@@deriving sexp, compare]
end

module Expr : sig
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

  val var : string -> t
  val const : int -> t
  val plus : t -> t -> t
  val minus : t -> t -> t
  val mult : t -> t -> t
  val block : t Val.t list -> t -> t
  val apply : t -> t list -> t
  val field : t -> string -> t
  val lambda : string list -> t -> t
  val pat_match : t -> (Deconstructor.t * t) list -> t
end

module Toplevel : sig
  type ('e, 't) decl =
    | FunExpression of 'e Function.t
    | TypeDefenition of ('t, string) Datatype.t
    | ActorDefition of 't Actor.t
    | HandlerDefinition of ('e, 't) Handler.t
  [@@deriving sexp, compare]
end

module Program : sig
  type ('e, 't) t = {
    functions : 'e Function.t list;
    types : ('t, string) Datatype.t list;
    actors : 't Actor.t list;
    handlers : ('e, 't) Handler.t list;
  }
  [@@deriving sexp, compare]

  val from_toplevels : ('a, 'b) Toplevel.decl list -> ('a, 'b) t
end
