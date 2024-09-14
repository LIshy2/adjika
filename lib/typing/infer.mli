exception NotActor

val infer_program :
  (Parsing.Ast.Expr.t, Parsing.Ast.TypeDecl.t) Parsing.Ast.Program.t ->
  Texp.t Texp.TProgram.program
