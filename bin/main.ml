open Core
open Pipeline
open Phase
open Analys.Capture

let llvm_init () =
  Llvm.enable_pretty_stacktrace ();
  Llvm.install_fatal_error_handler (fun msg ->
      print_endline ("FATAL_ERROR " ^ msg));
  ()

let capture_handler = function
  | UnknownName (name, ctx) ->
      let ctx_string = String.concat ~sep:"; " (Set.to_list ctx.locals) in
      print_endline ("Unbounded name " ^ name ^ "\n" ^ ctx_string ^ "\n")
  | UsedName name -> print_endline ("Name used more than once " ^ name ^ "\n\n")

let compiler_pipeline =
  pipeline (pure Parsing.Frontend.parse_toplevel)
  *> pure Typing.Infer.infer_program
  *> with_error Analys.Capture.capture_program capture_handler
  *> pure Analys.Address.connect
  *> pure (Codegen.Compiler.ProgramCompiler.compile_program "example")
  *> pure (Codegen.Compiler.Object.dump_object_file "tmp/output.o")

let () =
  let _ = llvm_init () in
  print_endline "Compiling tmp/example.adj";
  let input_channel = In_channel.create "tmp/example.adj" in
  let _ = execute input_channel compiler_pipeline in
  ()
