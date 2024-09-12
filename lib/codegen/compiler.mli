open Analys

module ProgramCompiler : sig
  val compile_program : string -> Address.AProgram.t -> Llvm.llmodule
end

module Object : sig
  val dump_object_file : string -> Llvm.llmodule -> unit
end
