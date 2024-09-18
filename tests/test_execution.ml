open Core
open Llvm_executionengine
open Parsing.Ast
open Typing
open Analys
open Foreign
open Ctypes
open! Llvm

let ast_from_string s =
  let tokens = Lexing.from_string s in
  try
    Ok (Program.from_toplevels (Parsing.Parser.prog Parsing.Lexer.read tokens))
  with Parsing.Parser.Error ->
    let pos = tokens.lex_curr_p in
    Error (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let _ = initialize ()

let open_result res =
  match res with
  | Result.Ok ok -> ok
  | Result.Error _ ->
      print_endline "auf";
      exit (-1)

let compile str =
  let ast = open_result (ast_from_string str) in
  let typed_ast = Infer.infer_program ast in
  let capture_ast = open_result (Capture.capture_program typed_ast) in
  let address_ast = Address.connect capture_ast in
  Codegen.Compiler.ProgramCompiler.compile_program "test" address_ast

let lambda_struct funptr =
  let lambda_struct = structure "lambda" in
  let field = field lambda_struct "fun" funptr in
  seal lambda_struct;
  (lambda_struct, field)

let%test_unit "const_int_fun" =
  let md = compile "fun constant() = 5" in
  let exec = create md in
  let constant_fun =
    get_function_address "constant"
      (funptr (ptr void @-> returning int64_t))
      exec
  in
  [%test_eq: int64] (constant_fun null) (Int64.of_int 5)

let%test_unit "one_arg_arithmetic_fun" =
  let md = compile "fun arithmetic(a) = a + (5 + 3) * 4" in
  let exec = create md in
  let constant_fun =
    get_function_address "arithmetic"
      (funptr (ptr void @-> int64_t @-> returning int64_t))
      exec
  in
  [%test_eq: int64] (constant_fun null (Int64.of_int 2)) (Int64.of_int 34)

let%test_unit "multi_arg_arithmetic_fun" =
  let md = compile "fun arithmetic(a, b, c) = a + 4 * b + (5 + c) * 12" in
  let exec = create md in
  let arithmetic_fun =
    get_function_address "arithmetic"
      (funptr
         (ptr void @-> int64_t @-> int64_t @-> int64_t @-> returning int64_t))
      exec
  in
  [%test_eq: int64]
    (arithmetic_fun null (Int64.of_int 1) (Int64.of_int 1) (Int64.of_int 1))
    (Int64.of_int 77)

let%test_unit "block_fun" =
  let md =
    compile
      "fun block() = {\n\
      \    val x = 5;\n\
      \    val y = 4;\n\
      \    val z = 2;\n\
      \    val result = (x + y) * z;\n\
      \    result\n\
      \  }"
  in
  let exec = create md in
  let block_fun =
    get_function_address "block" (funptr (ptr void @-> returning int64_t)) exec
  in
  [%test_eq: int64] (block_fun null) (Int64.of_int 18)

let%test_unit "return_fun" =
  let md = compile "fun return_fun() = fun(x, y) = x + y" in
  let exec = create md in
  let lambda_fun =
    get_function_address "lambda"
      (funptr (ptr void @-> int64_t @-> int64_t @-> returning int64_t))
      exec
  in
  [%test_eq: int64]
    (lambda_fun null (Int64.of_int 1) (Int64.of_int 2))
    (Int64.of_int 3)

let%test_unit "record_type" =
  let md =
    compile
      "\n\
      \  type rec = Point (x: int64, y: int64)\n\n\
      \  fun get_x(point) = point.x\n\n\
      \  fun get_y(point) = point.y\n\n\
      \  fun make_point(x, y) = Point(x, y)\n\
      \  "
  in
  let exec = create md in
  let point_struct = structure "rec" in
  let x_field = field point_struct "x" int64_t in
  let y_field = field point_struct "y" int64_t in
  seal point_struct;
  let p = make point_struct in
  setf p x_field (Int64.of_int 1);
  setf p y_field (Int64.of_int 3);
  let get_x =
    get_function_address "get_x"
      (funptr (ptr void @-> ptr point_struct @-> returning int64_t))
      exec
  in
  let get_y =
    get_function_address "get_y"
      (funptr (ptr void @-> ptr point_struct @-> returning int64_t))
      exec
  in
  let x = get_x null (addr p) in
  let y = get_y null (addr p) in
  [%test_eq: int64] x (Int64.of_int 1);
  [%test_eq: int64] y (Int64.of_int 3);

