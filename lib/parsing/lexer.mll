{
open Parser
open Lexing

exception SyntaxError of string
}

let int = '-'? ['0'-'9']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int      { CONST_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ':'      { COLON }
  | '.'      { DOT }
  | ','      { COMMA }
  | ';'      { SEMILICON }
  | '{'      { LEFT_BRACKET }
  | '}'      { RIGHT_BRACKET }
  | '('      { LEFT_BRACE }
  | ')'      { RIGHT_BRACE }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULT }
  | '='      { EQUALITY }
  | '|'      { PIPE }
  | "=>"     { ARROW }
  | "val"    { VAL_DEFINITION }
  | "fun"    { FUN_DEFINITION }
  | "match"  { MATCH }
  | "is"     { IS }
  | "type"   { TYPE_DEFINITION }
  | "int64"  { INT }
  | id       { ID (Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
