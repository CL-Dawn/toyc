exception Syntax_error
type token =
  | INT of (int)
  | ID of (string)
  | INT_TYPE
  | VOID_TYPE
  | IF
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | NOT
  | AND
  | OR
  | SEMICOLON
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
