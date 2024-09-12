open Core
open Pipeline
open Phase
open Analys.Capture

let llvm_init () = Llvm.enable_pretty_stacktrace ()

let capture_handler = function
  | UnknownName name -> print_endline ("Unbounded name " ^ name)
  | UsedName name -> print_endline ("Name used more than once " ^ name ^ "\n\n")

let compiler_pipeline args =
  pipeline (pure Parsing.Frontend.parse_toplevel)
  *> pure Typing.Infer.infer_program
  *> with_error Analys.Capture.capture_program capture_handler
  *> pure Analys.Address.connect
  *> pure
       (Codegen.Compiler.ProgramCompiler.compile_program
          (Cli.Arguments.input_file args))
  *> pure
       (Codegen.Compiler.Object.dump_object_file
          (Cli.Arguments.output_file args))

let () =
  let _ = llvm_init () in
  let args = Cli.cli_parse () in
  let input_channel = In_channel.create (Cli.Arguments.input_file args) in
  let _ = execute input_channel (compiler_pipeline args) in
  ()
