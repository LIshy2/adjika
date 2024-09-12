open Core

module Gen : sig
  val new_var : unit -> Typing.Type.mono

  val replace :
    (int, Typing.Type.mono, 'a) Map.t -> Typing.Type.mono -> Typing.Type.mono

  val instantiate : Typing.Type.poly -> Typing.Type.mono
end

module Constructor : sig
  type mono
  type poly

  val instantiate_con : poly -> mono
end

type t

exception UnknownName of string

val find : t -> string -> Typing.Type.poly
val add : string -> Typing.Type.poly -> t -> t
val empty : t

val of_list :
  locals:(string * Typing.Type.poly) list ->
  fields:(string * Typing.Type.poly) list ->
  constructors:(string * Constructor.mono) list ->
  t

val generealize : t -> Typing.Type.mono -> Typing.Type.poly
val constructor : t -> string -> Constructor.poly
