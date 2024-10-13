open Typing
open Capture
open Core

module Interactor : sig
  module Id : sig
    type t = Id of int [@@deriving sexp, compare]
    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
  end

  val id : int -> Id.t

  type 'e statement =
    | Spawn of { name : string; actor : 'e }
    | Val of Cexp.t Texp.TypedVal.t
    | Mutate of 'e
    | Send of { message : 'e; mail : 'e; destination : Id.t }

  type ('e, 't) handler = {
    message_type : 't;
    state : string;
    body : 'e statement list;
  }

  type ('e, 't) t = {
    actor : string;
    id : Id.t;
    handlers : ('e, 't) handler list;
  }

  val handler : 'a -> string -> 'b statement list -> ('b, 'a) handler
  val decl : string -> Id.t -> ('a, 'b) handler list -> ('a, 'b) t
  val message_type : ('a, 'b) t -> 'b
end

module AProgram : sig
  type t = {
    functions : Cexp.t Texp.TProgram.tfun list;
    types : (Type.mono, int) Parsing.Ast.Datatype.t list;
    actors : Type.mono Parsing.Ast.Actor.t list;
    interactors : (Cexp.t, Type.mono) Interactor.t list;
  }
end

module InteractorTag : sig
  type t = Id of (string * Type.mono)

  val actor : t -> string

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end

val connect : Cexp.t Texp.TProgram.program -> AProgram.t
