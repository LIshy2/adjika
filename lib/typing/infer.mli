open Parsing

exception NotActor

val infer_program :
  (Ast.Expr.t, Ast.Expr.t Ast.Val.t, Ast.TypeDecl.t) Ast.Program.t ->
  Texp.t Texp.TProgram.program
