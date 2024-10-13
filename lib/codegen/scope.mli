module Resources : sig
  type obj
  type t

  val concat : t -> t -> t
  val obj : Llvm.llvalue -> Bind.BindType.t -> obj
  val single : Llvm.llvalue -> Bind.BindType.t -> t
  val of_list : obj list -> t
  val empty : t
end

val desctruct : Resources.t -> Runtime.t -> Llvm.llbuilder -> unit
