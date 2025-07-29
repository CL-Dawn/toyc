{
  open Parser
  exception Lexical_error
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* 跳过空白 *)
  | "//" [^ '\n']* '\n' { token lexbuf }  (* 单行注释 *)
  | "/*" ([^ '*'] | '*' [^ '/'])* "*/" { token lexbuf }  (* 多行注释 *)
  | '-'? ['0'-'9']+ as num { INT (int_of_string num) }
  | "int" { INT_TYPE }
  | "void" { VOID_TYPE }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "return" { RETURN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MOD }
  | '=' { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '!' { NOT }
  | "&&" { AND }
  | "||" { OR }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID id }
  | eof { EOF }
  | _ { raise Lexical_error }