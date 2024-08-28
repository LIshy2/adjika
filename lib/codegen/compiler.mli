open Typing

module ProgramCompiler : sig
  val compile_program : string -> Texp.TypedProgram.t -> Llvm.llmodule
end

val dump_object_file : Llvm.llmodule -> string -> unit
