open Core
open Parsing.Ast
open Typing

module ProgramQuery = struct
  let get_function_type program name =
    let open Texp.TProgram in
    (List.find_exn program.functions ~f:(fun f -> String.equal f.name name))
      .fun_type

  let get_local_type program fun_name name =
    let open Texp.TProgram in
    let fn =
      List.find_exn program.functions ~f:(fun f -> String.equal f.name fun_name)
    in
    match fn.expr with
    | Texp.Block (defs, _) ->
        Texp.type_of
          (List.find_exn defs ~f:(fun def -> String.equal def.name name)).result
    | _ -> failwith "non_block"
end

let unwrap_ast = function
  | Result.Ok ast -> ast
  | Result.Error _ -> failwith "AST PARSING ERROR"

let ast_from_string s =
  let tokens = Lexing.from_string s in
  try
    Ok (Program.from_toplevels (Parsing.Parser.prog Parsing.Lexer.read tokens))
  with Parsing.Parser.Error ->
    let pos = tokens.lex_curr_p in
    Error (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let%test_unit "const_int_fun" =
  let ast = unwrap_ast (ast_from_string "fun constant() = 5") in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "constant" in
  [%test_eq: Type.poly] fun_tpe (Type.Mono (Type.Arrow ([], Type.Int)))

let%test_unit "id_fun" =
  let ast = unwrap_ast (ast_from_string "fun id(x) = x") in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "id" in
  [%test_eq: Type.poly] fun_tpe
    (Type.Quant ([ 2 ], Type.Arrow ([ Type.TypeVar 2 ], Type.TypeVar 2)))

let%test_unit "arithmetic_expr_fun" =
  let ast =
    unwrap_ast
      (ast_from_string "fun arithmetic(a, b, c) = a + 4 * b + (5 + c) * 12")
  in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "arithmetic" in
  [%test_eq: Type.poly] fun_tpe
    (Type.Mono (Type.Arrow ([ Type.Int; Type.Int; Type.Int ], Type.Int)))

let%test_unit "block_expr_fun" =
  let ast =
    unwrap_ast
      (ast_from_string
         "\n\
         \  fun arithmetic() = {\n\
         \    val x = 5;\n\
         \    val y = 4;\n\
         \    val z = 2;\n\
         \    val result = (x + y) * z;\n\
         \    result\n\
         \  }")
  in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "arithmetic" in
  let x_tpe = ProgramQuery.get_local_type typed_ast "arithmetic" "x" in
  let y_tpe = ProgramQuery.get_local_type typed_ast "arithmetic" "y" in
  let result_tpe =
    ProgramQuery.get_local_type typed_ast "arithmetic" "result"
  in
  [%test_eq: Type.poly] fun_tpe (Type.Mono (Type.Arrow ([], Type.Int)));
  [%test_eq: Type.poly] x_tpe (Type.Mono Type.Int);
  [%test_eq: Type.poly] y_tpe (Type.Mono Type.Int);
  [%test_eq: Type.poly] result_tpe (Type.Mono Type.Int)

let%test_unit "return_fun" =
  let ast =
    unwrap_ast (ast_from_string "fun return_fun() = fun(x, y) = x + y")
  in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "return_fun" in
  [%test_eq: Type.poly] fun_tpe
    (Type.Mono (Type.Arrow ([], Type.Arrow ([ Type.Int; Type.Int ], Type.Int))))

let%test_unit "returning_poly_lambda" =
  let ast = unwrap_ast (ast_from_string "fun poly_lambda() = fun(x) = x") in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "poly_lambda" in
  [%test_eq: Type.poly] fun_tpe
    (Type.Quant
       ( [ 12 ],
         Type.Arrow ([], Type.Arrow ([ Type.TypeVar 12 ], Type.TypeVar 12)) ))

let%test_unit "returning_poly_lambda_from_local" =
  let ast =
    unwrap_ast
      (ast_from_string
         "fun poly_lambda() = {\n    val id = fun(x) = x;\n    id\n  }")
  in
  let typed_ast = Infer.infer_program ast in
  let fun_tpe = ProgramQuery.get_function_type typed_ast "poly_lambda" in
  [%test_eq: Type.poly] fun_tpe
    (Type.Quant
       ( [ 15 ],
         Type.Arrow ([], Type.Arrow ([ Type.TypeVar 15 ], Type.TypeVar 15)) ))

let%test_unit "return_lambda_with_first_arg" =
  let ast =
    unwrap_ast
      (ast_from_string
         "\n\
         \      fun poly_lambda(k) = {\n\
         \        val id1 = fun (x) = x;\n\
         \        val id2 = fun (x) = x;\n\
         \        val j = k + 1;\n\
         \        id1\n\
         \      }\n\
         \      \n\
         \      fun po() = poly_lambda(32)\n\
         \      ")
  in
  let typed_ast = Infer.infer_program ast in
  let poly_tpe = ProgramQuery.get_function_type typed_ast "poly_lambda" in
  let po_tpe = ProgramQuery.get_function_type typed_ast "po" in
  [%test_eq: Type.poly] poly_tpe
    (Type.Quant
       ( [ 20 ],
         Type.Arrow
           ([ Type.Int ], Type.Arrow ([ Type.TypeVar 20 ], Type.TypeVar 20)) ));
  [%test_eq: Type.poly] po_tpe
    (Type.Quant
       ( [ 22 ],
         Type.Arrow ([], Type.Arrow ([ Type.TypeVar 22 ], Type.TypeVar 22)) ))

let%test_unit "record_type" =
  let ast =
    unwrap_ast
      (ast_from_string
         "\n\
         \  type rec = Point (x: int64, y: int64)\n\n\
         \  fun get_x(point) = point.x\n\n\
         \  fun get_y(point) = point.y\n\n\
         \  fun make_point(x, y) = Point(x, y)\n\
         \  ")
  in
  let typed_ast = Infer.infer_program ast in
  let get_x_tpe = ProgramQuery.get_function_type typed_ast "get_x" in
  let get_y_tpe = ProgramQuery.get_function_type typed_ast "get_y" in
  let make_point_tpe = ProgramQuery.get_function_type typed_ast "make_point" in
  [%test_eq: Type.poly] get_x_tpe
    (Type.Mono (Type.Arrow ([ Type.Named "rec" ], Type.Int)));
  [%test_eq: Type.poly] get_y_tpe
    (Type.Mono (Type.Arrow ([ Type.Named "rec" ], Type.Int)));
  [%test_eq: Type.poly] make_point_tpe
    (Type.Mono (Type.Arrow ([ Type.Int; Type.Int ], Type.Named "rec")))

let%test_unit "poly_record_type" =
  let ast =
    unwrap_ast
      (ast_from_string
         "\n\
         \  type rec[X, Y] = Point(x: X, y: Y)\n\n\
         \  fun get_x(point) = point.x\n\n\
         \  fun get_y(point) = point.y\n\n\
         \  fun make_point(x, y) = Point(x, y)\n\
         \  fun int_point(p1, p2) = p1.x + p2.y\n\
         \  fun int_point2(p1, p2) = p1.x + p1.y\n\
         \  fun int_point3(p1, p2) = p1.x + p1.y + p2.x + p2.y\n\
         \  ")
  in
  let typed_ast = Infer.infer_program ast in
  let get_x_tpe = ProgramQuery.get_function_type typed_ast "get_x" in
  let get_y_tpe = ProgramQuery.get_function_type typed_ast "get_y" in
  let make_point_tpe = ProgramQuery.get_function_type typed_ast "make_point" in
  let int_point_tpe = ProgramQuery.get_function_type typed_ast "int_point" in
  let int_point_tpe2 = ProgramQuery.get_function_type typed_ast "int_point2" in
  let int_point_tpe3 = ProgramQuery.get_function_type typed_ast "int_point3" in
  [%test_eq: Type.poly] get_x_tpe
    (Type.Quant
       ( [ 39; 41 ],
         Type.Arrow
           ( [ Type.Operator ("rec", [ Type.TypeVar 39; Type.TypeVar 41 ]) ],
             Type.TypeVar 39 ) ));
  [%test_eq: Type.poly] get_y_tpe
    (Type.Quant
       ( [ 44; 45 ],
         Type.Arrow
           ( [ Type.Operator ("rec", [ Type.TypeVar 45; Type.TypeVar 44 ]) ],
             Type.TypeVar 44 ) ));
  [%test_eq: Type.poly] make_point_tpe
    (Type.Quant
       ( [ 51; 52 ],
         Type.Arrow
           ( [ Type.TypeVar 51; Type.TypeVar 52 ],
             Type.Operator ("rec", [ Type.TypeVar 51; Type.TypeVar 52 ]) ) ));
  [%test_eq: Type.poly] int_point_tpe
    (Type.Quant
       ( [ 58; 60 ],
         Type.Arrow
           ( [
               Type.Operator ("rec", [ Type.Int; Type.TypeVar 58 ]);
               Type.Operator ("rec", [ Type.TypeVar 60; Type.Int ]);
             ],
             Type.Int ) ));
  [%test_eq: Type.poly] int_point_tpe2
    (Type.Quant
       ( [ 64 ],
         Type.Arrow
           ( [ Type.Operator ("rec", [ Type.Int; Type.Int ]); Type.TypeVar 64 ],
             Type.Int ) ));
  [%test_eq: Type.poly] int_point_tpe3
    (Type.Mono
       (Type.Arrow
          ( [
              Type.Operator ("rec", [ Type.Int; Type.Int ]);
              Type.Operator ("rec", [ Type.Int; Type.Int ]);
            ],
            Type.Int )))

let%test_unit "match_expr" =
  let _ =
    ast_from_string
      "type Geometry = Square (side1: int64, side2: int64) | Triangle (side1: \
       int64, side2: int64, side3: int64)\n\
      \       \n\
      \       fun detector(g) = \n\
      \         match g {\n\
      \           is Square(s1, s2) => 0\n\
      \           is Triangle(s1, s2, s3) => 1\n\
      \         }      \n\
      \       "
  in
  ()
