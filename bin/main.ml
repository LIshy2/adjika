open Core

let () =
  Llvm.enable_pretty_stacktrace ();
  Llvm.install_fatal_error_handler (fun msg ->
      print_endline ("FATAL_ERROR " ^ msg));
  let input_channel = In_channel.create "example.bf" in
  let ast = Parsing.Frontend.parse_toplevel input_channel in
  let cex_result = Parsing.Capture.capture_ast_program ast in
  match cex_result with
  | Result.Ok cex ->
      let typed_exp = Typing.Infer.infer_program cex in
      List.iter typed_exp.functions ~f:(fun t ->
          print_endline
            ("fun " ^ t.name ^ "("
            ^ String.concat ~sep:", " t.arguments
            ^ ") = " ^ Typing.Texp.show t.expr));
      let compiled_module =
        Codegen.Compiler.ProgramCompiler.compile_program "example" typed_exp
      in
      print_endline (Llvm.string_of_llmodule compiled_module);
      Codegen.Compiler.dump_object_file compiled_module "md";
      ()
  | Result.Error (UnknownName (name, ctx)) ->
      let ctx_string = String.concat ~sep:"; " (Set.to_list ctx.locals) in
      print_endline ("Unbounded name " ^ name ^ "\n" ^ ctx_string ^ "\n")
  | Result.Error (UsedName name) ->
      print_endline ("Name used more than once " ^ name ^ "\n\n")
