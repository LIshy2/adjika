type 'res t

exception Uninifiable of (Type.mono * Type.mono)

val unify : Type.mono -> Type.mono -> unit t

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
end

val run_substitution : 'a t -> 'a
val apply : Type.mono -> Type.mono t
val apply_poly : Type.poly -> Type.poly t
val apply_ast : Texp.t -> Texp.t t
val compose_list : 'a list -> f:('a -> 'b t) -> 'b list t
