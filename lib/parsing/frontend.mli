open Ast

val parse_toplevel : In_channel.t -> (Expr.t, Expr.t Val.t, TypeDecl.t) Program.t
