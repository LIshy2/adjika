open Ast

val parse_toplevel : In_channel.t -> (Expr.t, TypeDecl.t) Program.t
