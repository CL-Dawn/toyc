%{
  open Ast
  exception Syntax_error
%}
%token <int> INT
%token <string> ID
%token INT_TYPE VOID_TYPE
%token IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS TIMES DIV MOD
%token ASSIGN EQ NEQ LT GT LEQ GEQ NOT AND OR
%token SEMICOLON COMMA LPAREN RPAREN LBRACE RBRACE EOF

/* 优先级声明 */
%nonassoc IFX
%nonassoc ELSE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT UOP_MINUS UOP_PLUS

%start program
%type <Ast.program> program



%%
program:
  | function_defs EOF { $1 }

function_defs:
  | function_def { [$1] }
  | function_defs function_def { $1 @ [$2] }

function_def:
  | type_spec ID LPAREN param_list RPAREN block
      {
        {
          ret_type = $1;
          name = $2;
          params = $4;
          body = $6;
        }
      }

param_list:
  | /* empty */   { [] }
  | pparam_list    { $1 }

pparam_list:
  | param                   { [$1] }
  | pparam_list COMMA param  { $1 @ [$3] }

type_spec:
  | INT_TYPE { Int }
  | VOID_TYPE { Void }

param:
  | INT_TYPE ID { (Int, $2) }

block:
  | LBRACE statements RBRACE { Block $2 }

statements:
  | /* empty */ { [] }
  | statements stmt { $1 @ [$2] }

stmt:
  | block { $1 }
  | SEMICOLON { EmptyStmt }
  | expr SEMICOLON { ExprStmt $1 }
  | INT_TYPE ID ASSIGN expr SEMICOLON { VarDecl ($2, $4) }
  | ID ASSIGN expr SEMICOLON { Assign ($1, $3) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If ($3, $5, Some $7) }
  | IF LPAREN expr RPAREN stmt %prec IFX { If ($3, $5, None) }
  | WHILE LPAREN expr RPAREN stmt { While ($3, $5) }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  | RETURN SEMICOLON { Return None }
  | RETURN expr SEMICOLON { Return (Some $2) }

expr:
  | expr TIMES expr { Binop ($1, Mul, $3) }  /* 修正为 Binop (e1, op, e2) 形式 */
  | expr DIV expr   { Binop ($1, Div, $3) }
  | expr MOD expr   { Binop ($1, Mod, $3) }
  | expr PLUS expr  { Binop ($1, Add, $3) }
  | expr MINUS expr { Binop ($1, Sub, $3) }
  | expr LEQ expr   { Binop ($1, Leq, $3) }
  | expr GEQ expr   { Binop ($1, Geq, $3) }
  | expr GT expr    { Binop ($1, Gt, $3) }
  | expr LT expr    { Binop ($1, Lt, $3) }
  | expr EQ expr    { Binop ($1, Eq, $3) }
  | expr NEQ expr   { Binop ($1, Neq, $3) }
  | expr AND expr   { Binop ($1, And, $3) }
  | expr OR expr    { Binop ($1, Or, $3) }
  | NOT expr        { Unop (Not, $2) }
  | MINUS expr      { Unop (Neg, $2) } %prec UOP_MINUS
  | PLUS expr       { Unop (Pos, $2) } %prec UOP_PLUS
  | primary        { $1 }

primary:
  | ID   { Var $1 } 
  | INT   { IntLit $1 }  
  | LPAREN expr RPAREN   { $2 }
  | ID LPAREN arg_list_opt RPAREN   { Call ($1, $3) }

arg_list_opt:
  | /* empty */   { [] }
  | arg_list      { $1 }

arg_list:
  | expr                { [$1] }
  | arg_list COMMA expr { $1 @ [$3] }

%%