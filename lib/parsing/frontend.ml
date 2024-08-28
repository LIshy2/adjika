open Lexing
open Core



let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  
let parse_toplevel inx =
  let tokens = Lexing.from_channel inx in
  try Ast.Program.from_toplevels (Parser.prog Lexer.read tokens)
  with Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position tokens;
    exit (-1)

