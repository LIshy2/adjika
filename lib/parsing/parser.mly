%token <string> ID
%token <int> CONST_INT
%token <float> CONST_FLOAT
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_SQBR
%token RIGHT_SQBR
%token EQUALITY
%token ARROW
%token COLON
%token DOT
%token SEMILICON
%token COMMA  
%token FUN_DEFINITION
%token VAL_DEFINITION
%token TYPE_DEFINITION
%token ACTOR_DEFINITION
%token HANDLER_DEFINITION
%token MAILBOX
%token MATCH
%token TO
%token WITH
%token SPAWN
%token MUTATE
%token SEND
%token IS
%token INT
%token FLOAT
%token BOOL
%token PIPE
%token PLUS
%token MINUS
%token MULT
%token EOF

%left EQUALITY
%right LEFT_BRACE
%left PLUS MINUS
%left MULT
%left DOT




%start <(Ast.Expr.t, Ast.TypeDecl.t) Ast.Toplevel.decl list> prog
%%

prog:
  declarations = list(toplevel); EOF { declarations }


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
  | fund = fun_definition { Ast.Toplevel.FunExpression fund }
  | typd = type_definition { Ast.Toplevel.TypeDefenition typd }
  | actd = actor_definition { Ast.Toplevel.ActorDefition actd }
  | hand = handler_definition { Ast.Toplevel.HandlerDefinition hand }
  ;

val_definition:
  VAL_DEFINITION; name = ID; EQUALITY; exp = expr; SEMILICON { Ast.Val.decl name exp } ;

fun_definition:
  FUN_DEFINITION; name = ID; LEFT_BRACE; args = separated_list(COMMA, ID); RIGHT_BRACE; EQUALITY; result = expr { Ast.Function.decl name args result } ;
 
type_definition:
  | TYPE_DEFINITION; name = ID; EQUALITY; constructors = separated_list(PIPE, constructor_definition) { Ast.Datatype.mono name constructors }
  | TYPE_DEFINITION; name = ID; LEFT_SQBR; args = separated_list(COMMA, ID) RIGHT_SQBR; EQUALITY; constructors = separated_list(PIPE, constructor_definition) { Ast.Datatype.operator name args constructors }
  ; 

constructor_definition:
  name = ID; LEFT_BRACE; fields = separated_list(COMMA, typed_field); RIGHT_BRACE { Ast.Datatype.constructor name fields } 
  ;

typed_field:
  name = ID; COLON; t = tpe { (name, t) }
  ;

tpe:
  | name = ID { Ast.TypeDecl.Custom (name) }
  | MAILBOX; LEFT_SQBR; name = ID; RIGHT_SQBR { Ast.TypeDecl.MailBox (name) }
  | name = ID; LEFT_SQBR; args = separated_list(COMMA, tpe) RIGHT_SQBR { Ast.TypeDecl.Operator (name, args) }
  | INT { Ast.TypeDecl.Int }
  | FLOAT { Ast.TypeDecl.Float }
  | BOOL { Ast.TypeDecl.Bool }
  | LEFT_BRACE; args = separated_list(COMMA, tpe); RIGHT_BRACE; ARROW; result = tpe { Ast.TypeDecl.Arrow (args, result) }
  ;

deconstructor:
  | name = ID { Ast.Deconstructor.var name}
  | name = ID; LEFT_BRACE; subs = separated_list(COMMA, deconstructor); RIGHT_BRACE { Ast.Deconstructor.constructor name subs}
  ;

actor_definition:
  | ACTOR_DEFINITION; name = ID; LEFT_BRACKET; states = list(actor_state); RIGHT_BRACKET { Ast.Actor.{name; states} }
  ;

actor_state:
  name = ID; LEFT_BRACE; fields = separated_list(COMMA, typed_field) RIGHT_BRACE; SEMILICON { Ast.Actor.{name; fields} }

handler_definition:
  | HANDLER_DEFINITION; message_type = tpe;  WITH; state = ID;  EQUALITY; body = handler_body { Ast.Handler.decl message_type state body }
  ; 

handler_expr:
  | v = val_definition; SEMILICON { let open Ast.Val in Ast.Handler.local v.name v.result }
  | VAL_DEFINITION; name = ID; EQUALITY; SPAWN; actor = expr; SEMILICON { Ast.Handler.spawn name actor }
  | SEND; message = expr; TO; mail = expr; SEMILICON { Ast.Handler.send message mail }
  | MUTATE; constructor = expr; SEMILICON { Ast.Handler.mutate constructor }
  ;


handler_body:
  LEFT_BRACKET; statements = list(handler_expr); RIGHT_BRACKET { statements }
  