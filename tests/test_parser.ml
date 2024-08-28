open Core
open Parsing

let ast_from_string s =
  let tokens = Lexing.from_string s in
  try Ok (Ast.Program.from_toplevels (Parser.prog Lexer.read tokens))
  with Parser.Error ->
    let pos = tokens.lex_curr_p in
    Error (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let%test_unit "const_int_fun" =
  let ast = ast_from_string "fun constant() = 5" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               { name = "constant"; arguments = []; result = Ast.Expr.Const 5 };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "id_fun" =
  let ast = ast_from_string "fun id(x) = x" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               { name = "id"; arguments = [ "x" ]; result = Ast.Expr.Var "x" };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "arithmetic_expr_fun" =
  let ast = ast_from_string "fun arithmetic() = 12 + 4 * 3 + (5 + 2) * 12" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Ast.Expr.Oper
                     ( Ast.BinOp.Plus,
                       Ast.Expr.Oper
                         ( Ast.BinOp.Plus,
                           Ast.Expr.Const 12,
                           Ast.Expr.Oper
                             (Ast.BinOp.Mult, Ast.Expr.Const 4, Ast.Expr.Const 3)
                         ),
                       Ast.Expr.Oper
                         ( Ast.BinOp.Mult,
                           Ast.Expr.Oper
                             (Ast.BinOp.Plus, Ast.Expr.Const 5, Ast.Expr.Const 2),
                           Ast.Expr.Const 12 ) );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "block_expr_fun" =
  let ast =
    ast_from_string
      "\n\
      \  fun arithmetic() = {\n\
      \    val x = 5;\n\
      \    val y = 4;\n\
      \    val z = 2;\n\
      \    val result = (x + y) * z;\n\
      \    result\n\
      \  }"
  in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Ast.Expr.Block
                     ( [
                         Ast.Symbol.{ name = "x"; result = Ast.Expr.Const 5 };
                         Ast.Symbol.{ name = "y"; result = Ast.Expr.Const 4 };
                         Ast.Symbol.{ name = "z"; result = Ast.Expr.Const 2 };
                         Ast.Symbol.
                           {
                             name = "result";
                             result =
                               Ast.Expr.Oper
                                 ( Ast.BinOp.Mult,
                                   Ast.Expr.Oper
                                     ( Ast.BinOp.Plus,
                                       Ast.Expr.Var "x",
                                       Ast.Expr.Var "y" ),
                                   Ast.Expr.Var "z" );
                           };
                       ],
                       Ast.Expr.Var "result" );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "lambda_fun" =
  let ast = ast_from_string "fun arithmetic() = fun(x, y) = x + y" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Ast.Expr.Lambda
                     ( [ "x"; "y" ],
                       Ast.Expr.Oper
                         (Ast.BinOp.Plus, Ast.Expr.Var "x", Ast.Expr.Var "y") );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "lambda_fun" =
  let ast = ast_from_string "fun arithmetic() = fun(x, y) = x + y" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Ast.Expr.Lambda
                     ( [ "x"; "y" ],
                       Ast.Expr.Oper
                         (Ast.BinOp.Plus, Ast.Expr.Var "x", Ast.Expr.Var "y") );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "record_type" =
  let ast = ast_from_string "type rec = Point (x: int64, y: int64)" in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.TypeDefenition
             Ast.Symbol.
               {
                 name = "rec";
                 constructors =
                   [
                     Ast.Symbol.
                       {
                         name = "Point";
                         fields =
                           [ ("x", Ast.TypeDecl.Int); ("y", Ast.TypeDecl.Int) ];
                       };
                   ];
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "sum_type" =
  let ast =
    ast_from_string
      "type Geometry = Square (side1: int64, side2: int64) | Triangle (side1: \
       int64, side2: int64, side3: int64)"
  in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.TypeDefenition
             Ast.Symbol.
               {
                 name = "Geometry";
                 constructors =
                   [
                     Ast.Symbol.
                       {
                         name = "Square";
                         fields =
                           [
                             ("side1", Ast.TypeDecl.Int);
                             ("side2", Ast.TypeDecl.Int);
                           ];
                       };
                     Ast.Symbol.
                       {
                         name = "Triangle";
                         fields =
                           [
                             ("side1", Ast.TypeDecl.Int);
                             ("side2", Ast.TypeDecl.Int);
                             ("side3", Ast.TypeDecl.Int);
                           ];
                       };
                   ];
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "match_expr" =
  let ast =
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
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.TypeDefenition
             Ast.Symbol.
               {
                 name = "Geometry";
                 constructors =
                   [
                     Ast.Symbol.
                       {
                         name = "Square";
                         fields =
                           [
                             ("side1", Ast.TypeDecl.Int);
                             ("side2", Ast.TypeDecl.Int);
                           ];
                       };
                     Ast.Symbol.
                       {
                         name = "Triangle";
                         fields =
                           [
                             ("side1", Ast.TypeDecl.Int);
                             ("side2", Ast.TypeDecl.Int);
                             ("side3", Ast.TypeDecl.Int);
                           ];
                       };
                   ];
               };
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "detector";
                 arguments = [ "g" ];
                 result =
                   Ast.Expr.PatMatch
                     ( Ast.Expr.Var "g",
                       [
                         ( Ast.Deconstructor.Constructor
                             ( "Square",
                               [
                                 Ast.Deconstructor.Var "s1";
                                 Ast.Deconstructor.Var "s2";
                               ] ),
                           Ast.Expr.Const 0 );
                         ( Ast.Deconstructor.Constructor
                             ( "Triangle",
                               [
                                 Ast.Deconstructor.Var "s1";
                                 Ast.Deconstructor.Var "s2";
                                 Ast.Deconstructor.Var "s3";
                               ] ),
                           Ast.Expr.Const 1 );
                       ] );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected

let%test_unit "field_access" =
  let ast =
    ast_from_string
      "\n\
      \  type rec = Point (x: int64, y: int64)\n\n\
      \  fun point_sum(a, b) = \n\
      \    Point(a.x + b.x, a.y + b.y)\n\
      \  "
  in
  let expected =
    Ok
      (Ast.Program.from_toplevels
         [
           Ast.Toplevel.TypeDefenition
             Ast.Symbol.
               {
                 name = "rec";
                 constructors =
                   [
                     Ast.Symbol.
                       {
                         name = "Point";
                         fields =
                           [ ("x", Ast.TypeDecl.Int); ("y", Ast.TypeDecl.Int) ];
                       };
                   ];
               };
           Ast.Toplevel.FunExpression
             Ast.Symbol.
               {
                 name = "point_sum";
                 arguments = [ "a"; "b" ];
                 result =
                   Ast.Expr.Apply
                     ( Ast.Expr.Var "Point",
                       [
                         Ast.Expr.Oper
                           ( Ast.BinOp.Plus,
                             Ast.Expr.Field (Ast.Expr.Var "a", "x"),
                             Ast.Expr.Field (Ast.Expr.Var "b", "x") );
                         Ast.Expr.Oper
                           ( Ast.BinOp.Plus,
                             Ast.Expr.Field (Ast.Expr.Var "a", "y"),
                             Ast.Expr.Field (Ast.Expr.Var "b", "y") );
                       ] );
               };
         ])
  in
  [%test_eq:
    ((Ast.Expr.expr, Ast.TypeDecl.t) Ast.Program.t, string * int * int) result]
    ast expected
