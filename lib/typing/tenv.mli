open Core

module Gen : sig
  type type_id = int

  val new_var : unit -> Type.mono
  val replace : (int, Type.mono, 'a) Map.t -> Type.mono -> Type.mono
  val instantiate : Type.poly -> Type.mono
end

module Constructor : sig
  type mono
  type poly

  val poly : Type.poly -> Type.mono list -> poly
  val mono : Type.mono -> Type.mono list -> mono
  val instantiate : poly -> mono
  val instance : mono -> Type.mono
  val fields : mono -> Type.mono list
end

type t

exception UnknownName of string

val find : t -> string -> Type.poly
val find_field : t -> string -> Type.poly
val add : string -> Type.poly -> t -> t
val empty : t

val of_list :
  locals:(string * Type.poly) list ->
  fields:(string * Type.poly) list ->
  constructors:(string * Constructor.poly) list ->
  t

val generealize : t -> Type.mono -> Type.poly
val constructor : t -> string -> Constructor.poly
val apply_to_locals : (Type.poly -> Type.poly) -> t -> t
val apply_to_fields : (Type.poly -> Type.poly) -> t -> t
