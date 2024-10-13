type t

val declare_in_module : Llvm.llcontext -> Llvm.llmodule -> t
val declare_main : Llvm.llcontext -> Llvm.llmodule -> unit

val build_send_int :
  t ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue

val build_send_ptr :
  t ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue

val build_send :
  t ->
  Typing.Type.mono ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue

val build_spawn : t -> Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue

val build_allocate :
  t ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue

val build_dup :
  t ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue

val build_dedup :
  t ->
  Llvm.llvalue ->
  string ->
  Llvm.llbuilder ->
  Llvm.llvalue