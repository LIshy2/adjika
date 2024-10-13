open Core
open Parsing.Ast

let ast_from_string s =
  let tokens = Lexing.from_string s in
  try
    Ok (Program.from_toplevels (Parsing.Parser.prog Parsing.Lexer.read tokens))
  with Parsing.Parser.Error ->
    let pos = tokens.lex_curr_p in
    Error (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let%test_unit "const_int_fun" =
  let ast = ast_from_string "fun constant() = 5" in
  let expected =
    Ok
      (Program.from_toplevels
         [ FunExpression (Function.decl "constant" [] (Expr.Const 5)) ])
  in
  [%test_eq: ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result] ast
    expected

let%test_unit "id_fun" =
  let ast = ast_from_string "fun id(x) = x" in
  let expected =
    Ok
      (Program.from_toplevels
         [ Toplevel.FunExpression (Function.decl "id" [ "x" ] (Expr.Var "x")) ])
  in
  [%test_eq: ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result] ast
    expected

let%test_unit "arithmetic_expr_fun" =
  let ast = ast_from_string "fun arithmetic() = 12 + 4 * 3 + (5 + 2) * 12" in
  let expected =
    Ok
      (Program.from_toplevels
         [
           Toplevel.FunExpression
             (Function.decl "arithmetic" []
                (Expr.Oper
                   ( BinOp.Plus,
                     Expr.Oper
                       ( BinOp.Plus,
                         Expr.Const 12,
                         Expr.Oper (BinOp.Mult, Expr.Const 4, Expr.Const 3) ),
                     Expr.Oper
                       ( BinOp.Mult,
                         Expr.Oper (BinOp.Plus, Expr.Const 5, Expr.Const 2),
                         Expr.Const 12 ) )));
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
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
      (Program.from_toplevels
         [
           Toplevel.FunExpression
             Function.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Expr.Block
                     ( [
                         Val.{ name = "x"; result = Expr.Const 5 };
                         Val.{ name = "y"; result = Expr.Const 4 };
                         Val.{ name = "z"; result = Expr.Const 2 };
                         Val.
                           {
                             name = "result";
                             result =
                               Expr.Oper
                                 ( BinOp.Mult,
                                   Expr.Oper
                                     (BinOp.Plus, Expr.Var "x", Expr.Var "y"),
                                   Expr.Var "z" );
                           };
                       ],
                       Expr.Var "result" );
               };
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
    ast expected

let%test_unit "lambda_fun" =
  let ast = ast_from_string "fun arithmetic() = fun(x, y) = x + y" in
  let expected =
    Ok
      (Program.from_toplevels
         [
           Toplevel.FunExpression
             Function.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Expr.Lambda
                     ( [ "x"; "y" ],
                       Expr.Oper (BinOp.Plus, Expr.Var "x", Expr.Var "y") );
               };
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
    ast expected

let%test_unit "lambda_fun" =
  let ast = ast_from_string "fun arithmetic() = fun(x, y) = x + y" in
  let expected =
    Ok
      (Program.from_toplevels
         [
           Toplevel.FunExpression
             Function.
               {
                 name = "arithmetic";
                 arguments = [];
                 result =
                   Expr.Lambda
                     ( [ "x"; "y" ],
                       Expr.Oper (BinOp.Plus, Expr.Var "x", Expr.Var "y") );
               };
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
    ast expected

let%test_unit "record_type" =
  let ast = ast_from_string "type rec = Point (x: int64, y: int64)" in
  let expected =
    Ok
      (Program.from_toplevels
         [
           Toplevel.TypeDefenition
             Datatype.(
               Mono
                 {
                   name = "rec";
                   constructors =
                     [
                       Datatype.
                         {
                           name = "Point";
                           fields = [ ("x", TypeDecl.Int); ("y", TypeDecl.Int) ];
                         };
                     ];
                 });
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
    ast expected

let%test_unit "sum_type" =
  let ast =
    ast_from_string
      "type Geometry = Square (side1: int64, side2: int64) | Triangle (side1: \
       int64, side2: int64, side3: int64)"
  in
  let expected =
    Ok
      (Program.from_toplevels
         [
           Toplevel.TypeDefenition
             Datatype.(
               Mono
                 {
                   name = "Geometry";
                   constructors =
                     [
                       Datatype.
                         {
                           name = "Square";
                           fields =
                             [
                               ("side1", TypeDecl.Int); ("side2", TypeDecl.Int);
                             ];
                         };
                       Datatype.
                         {
                           name = "Triangle";
                           fields =
                             [
                               ("side1", TypeDecl.Int);
                               ("side2", TypeDecl.Int);
                               ("side3", TypeDecl.Int);
                             ];
                         };
                     ];
                 });
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
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
      (Program.from_toplevels
         [
           Toplevel.TypeDefenition
             Datatype.(
               Mono
                 {
                   name = "Geometry";
                   constructors =
                     [
                       Datatype.
                         {
                           name = "Square";
                           fields =
                             [
                               ("side1", TypeDecl.Int); ("side2", TypeDecl.Int);
                             ];
                         };
                       Datatype.
                         {
                           name = "Triangle";
                           fields =
                             [
                               ("side1", TypeDecl.Int);
                               ("side2", TypeDecl.Int);
                               ("side3", TypeDecl.Int);
                             ];
                         };
                     ];
                 });
           Toplevel.FunExpression
             Function.
               {
                 name = "detector";
                 arguments = [ "g" ];
                 result =
                   Expr.PatMatch
                     ( Expr.Var "g",
                       [
                         ( Deconstructor.Constructor
                             ( "Square",
                               [
                                 Deconstructor.Var "s1"; Deconstructor.Var "s2";
                               ] ),
                           Expr.Const 0 );
                         ( Deconstructor.Constructor
                             ( "Triangle",
                               [
                                 Deconstructor.Var "s1";
                                 Deconstructor.Var "s2";
                                 Deconstructor.Var "s3";
                               ] ),
                           Expr.Const 1 );
                       ] );
               };
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
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
      (Program.from_toplevels
         [
           Toplevel.TypeDefenition
             Datatype.(
               Mono
                 {
                   name = "rec";
                   constructors =
                     [
                       Datatype.
                         {
                           name = "Point";
                           fields = [ ("x", TypeDecl.Int); ("y", TypeDecl.Int) ];
                         };
                     ];
                 });
           Toplevel.FunExpression
             Function.
               {
                 name = "point_sum";
                 arguments = [ "a"; "b" ];
                 result =
                   Expr.Apply
                     ( Expr.Var "Point",
                       [
                         Expr.Oper
                           ( BinOp.Plus,
                             Expr.Field (Expr.Var "a", "x"),
                             Expr.Field (Expr.Var "b", "x") );
                         Expr.Oper
                           ( BinOp.Plus,
                             Expr.Field (Expr.Var "a", "y"),
                             Expr.Field (Expr.Var "b", "y") );
                       ] );
               };
         ])
  in
  [%test_eq:
    ((Expr.t, Expr.t Val.t, TypeDecl.t) Program.t, string * int * int) result]
    ast expected
