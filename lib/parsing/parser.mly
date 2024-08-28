%token <string> ID
%token <int> CONST_INT
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token EQUALITY
%token ARROW
%token COLON
%token DOT
%token SEMILICON
%token COMMA  
%token FUN_DEFINITION
%token VAL_DEFINITION
%token TYPE_DEFINITION
%token MATCH
%token IS
%token INT
%token PIPE
%token PLUS
%token MINUS
%token MULT
%token EOF

%left PLUS MINUS
%left MULT


%start <(Ast.Expr.expr, Ast.TypeDecl.t) Ast.Toplevel.decl list> prog
%%

prog:
  | declarations = list(toplevel); EOF { declarations }
  | EOF           { [] }
  ;


expr:
  | name = ID { Ast.Expr.var name }
  | const_int = CONST_INT { Ast.Expr.const const_int }
  | lhs = expr; PLUS; rhs = expr { Ast.Expr.plus lhs rhs }
  | lhs = expr; MINUS; rhs = expr { Ast.Expr.minus lhs rhs }
  | lhs = expr; MULT; rhs = expr { Ast.Expr.mult lhs rhs }
  | LEFT_BRACE; e = expr; RIGHT_BRACE { e }
  | name = expr; LEFT_BRACE; arguments = separated_list(COMMA, expr); RIGHT_BRACE { Ast.Expr.apply name arguments }
  | FUN_DEFINITION; LEFT_BRACE; arguments = separated_list(COMMA, ID); RIGHT_BRACE; EQUALITY; result = expr; { Ast.Expr.lambda arguments result }
  | LEFT_BRACKET; defs = list(val_definition); result = expr; RIGHT_BRACKET { Ast.Expr.block defs result } 
  | struct_val = expr; DOT; name = ID { Ast.Expr.field struct_val name }
  | MATCH; arg = expr; LEFT_BRACKET; cases = list(case); RIGHT_BRACKET { Ast.Expr.pat_match arg cases }
  ;


case:
  IS; d = deconstructor; ARROW; e = expr { (d, e)};


toplevel: 
  | fund = fun_definition { fund }
  | typd = type_definition { typd }
  ;

val_definition:
  VAL_DEFINITION; name = ID; EQUALITY; exp = expr; SEMILICON { Ast.Toplevel.valdecl name exp } ;

fun_definition:
  FUN_DEFINITION; name = ID; LEFT_BRACE; args = separated_list(COMMA, ID); RIGHT_BRACE; EQUALITY; result = expr { Ast.Toplevel.fundecl name args result } ;
 
type_definition:
  TYPE_DEFINITION; name = ID; EQUALITY; constructors = separated_list(PIPE, constructor_definition) { Ast.Toplevel.typedecl name constructors } ;

constructor_definition:
  name = ID; LEFT_BRACE; fields = separated_list(COMMA, typed_field); RIGHT_BRACE { Ast.Toplevel.condecl name fields } 
  ;

typed_field:
  name = ID; COLON; t = tpe { (name, t) }
  ;

tpe:
  | INT { Ast.TypeDecl.Int }
  | LEFT_BRACE; args = separated_list(COMMA, tpe); RIGHT_BRACE; ARROW; result = tpe { Ast.TypeDecl.Arrow (args, result) }
  ;

deconstructor:
  | name = ID { Ast.Deconstructor.var name}
  | name = ID; LEFT_BRACE; subs = separated_list(COMMA, deconstructor); RIGHT_BRACE; { Ast.Deconstructor.constructor name subs}
  ;