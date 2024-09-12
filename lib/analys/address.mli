module Interactor : sig
  module Id : sig
    type t = Id of int [@@deriving sexp, compare]
    type comparator_witness

    val comparator : (t, comparator_witness) Base.Comparator.t
  end

  val id : int -> Id.t

  type 'e statement =
    | Spawn of { name : string; actor : 'e }
    | Val of { name : string; result : 'e }
    | Mutate of 'e
    | Send of { message : 'e; mail : 'e; destination : Id.t }
  [@@deriving sexp, compare]

  type ('e, 't) handler = {
    message_type : 't;
    state : string;
    body : 'e statement list;
  }
  [@@deriving sexp, compare]

  type ('e, 't) t = {
    actor : string;
    id : Id.t;
    handlers : ('e, 't) handler list;
  }
  [@@deriving sexp, compare]

  val compare :
    ('e -> 'e -> int) -> ('t -> 't -> int) -> ('e, 't) t -> ('e, 't) t -> int

  val handler : 'a -> string -> 'b statement list -> ('b, 'a) handler
  val decl : string -> Id.t -> ('a, 'b) handler list -> ('a, 'b) t
  val message_type : ('a, 'b) t -> 'b
end

module AProgram : sig
  type t = {
    functions : Capture.Cexp.t Typing.Texp.TProgram.tfun list;
    types : (Typing.Type.mono, int) Parsing.Ast.Datatype.t list;
    actors : Typing.Type.mono Parsing.Ast.Actor.t list;
    interactors : (Capture.Cexp.t, Typing.Type.mono) Interactor.t list;
  }
end

module InteractorTag : sig
  type t = Id of (string * Typing.Type.mono)

  val actor : t -> string

  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.t
end

val handler_direction :
  (InteractorTag.t, Interactor.Id.t, 'a) Base.Map.t ->
  (Capture.Cexp.t, 'b) Parsing.Ast.Handler.t ->
  (Capture.Cexp.t, 'b) Interactor.handler

val connect : Capture.Cexp.t Typing.Texp.TProgram.t -> AProgram.t
