open Core

exception Uninifiable of (Typing.Type.mono * Typing.Type.mono)
exception NotActor

val infer_program :
  (Parsing.Ast.Expr.t, Parsing.Ast.TypeDecl.t) Parsing.Ast.Program.t ->
  Texp.expr Texp.TProgram.t
