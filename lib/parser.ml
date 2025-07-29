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

open Parsing;;
let _ = parse_error;;
# 2 "lib/parser.mly"
  open Ast
  exception Syntax_error
# 41 "lib/parser.ml"
let yytransl_const = [|
  259 (* INT_TYPE *);
  260 (* VOID_TYPE *);
  261 (* IF *);
  262 (* ELSE *);
  263 (* WHILE *);
  264 (* BREAK *);
  265 (* CONTINUE *);
  266 (* RETURN *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIV *);
  271 (* MOD *);
  272 (* ASSIGN *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* GT *);
  277 (* LEQ *);
  278 (* GEQ *);
  279 (* NOT *);
  280 (* AND *);
  281 (* OR *);
  282 (* SEMICOLON *);
  283 (* COMMA *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* LBRACE *);
  287 (* RBRACE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\005\000\005\000\007\000\007\000\
\004\000\004\000\008\000\006\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\013\000\
\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\006\000\000\000\001\000\001\000\003\000\
\001\000\001\000\002\000\003\000\000\000\002\000\001\000\001\000\
\002\000\005\000\004\000\007\000\005\000\005\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\002\000\001\000\001\000\001\000\003\000\004\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\010\000\052\000\000\000\002\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\000\000\007\000\
\011\000\000\000\000\000\013\000\004\000\008\000\000\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\012\000\015\000\014\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\023\000\024\000\
\000\000\025\000\000\000\042\000\041\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\046\000\000\000\000\000\
\027\000\028\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\047\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\022\000\000\000\020\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\014\000\038\000\015\000\016\000\
\023\000\039\000\040\000\041\000\072\000\073\000"

let yysindex = "\255\255\
\037\255\000\000\000\000\000\000\000\000\001\000\000\000\020\255\
\000\000\000\000\035\255\058\255\064\255\039\255\049\255\000\000\
\000\000\040\255\058\255\000\000\000\000\000\000\041\255\000\000\
\031\255\076\255\063\255\069\255\073\255\075\255\116\255\123\255\
\123\255\123\255\000\000\123\255\000\000\000\000\000\000\008\000\
\000\000\123\255\123\255\091\255\123\255\123\255\000\000\000\000\
\081\255\000\000\024\000\000\000\000\000\000\000\160\255\123\255\
\123\255\123\255\123\255\123\255\123\255\123\255\123\255\123\255\
\123\255\123\255\123\255\123\255\000\000\040\000\088\000\060\255\
\089\255\123\255\179\255\198\255\000\000\000\000\011\255\011\255\
\000\000\000\000\000\000\181\000\181\000\043\255\043\255\043\255\
\043\255\169\000\103\000\000\000\000\000\123\255\056\000\103\255\
\103\255\088\000\000\000\113\255\000\000\103\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\092\255\000\000\000\000\093\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\072\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\094\255\000\000\000\000\000\000\000\000\000\000\
\141\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\235\254\000\000\
\101\255\000\000\000\000\000\000\000\000\000\000\213\255\245\255\
\000\000\000\000\000\000\250\254\150\000\226\255\111\000\124\000\
\137\000\061\255\067\255\000\000\000\000\000\000\000\000\000\000\
\000\000\033\255\000\000\072\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\114\000\000\000\000\000\118\000\000\000\113\000\
\000\000\169\255\227\255\000\000\000\000\000\000"

let yytablesize = 459
let yytable = "\001\000\
\009\000\051\000\052\000\053\000\054\000\050\000\055\000\050\000\
\100\000\101\000\036\000\036\000\070\000\071\000\103\000\075\000\
\076\000\036\000\036\000\036\000\036\000\011\000\036\000\058\000\
\059\000\060\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\003\000\
\004\000\024\000\025\000\026\000\095\000\027\000\042\000\028\000\
\029\000\030\000\031\000\032\000\033\000\056\000\057\000\058\000\
\059\000\060\000\043\000\051\000\013\000\051\000\012\000\034\000\
\098\000\017\000\035\000\018\000\036\000\020\000\020\000\037\000\
\021\000\021\000\021\000\019\000\021\000\044\000\021\000\021\000\
\021\000\021\000\021\000\021\000\038\000\038\000\038\000\038\000\
\093\000\038\000\045\000\039\000\039\000\039\000\021\000\039\000\
\046\000\021\000\047\000\021\000\048\000\021\000\021\000\024\000\
\025\000\026\000\074\000\027\000\043\000\028\000\029\000\030\000\
\031\000\032\000\033\000\094\000\024\000\049\000\102\000\010\000\
\005\000\006\000\048\000\024\000\049\000\034\000\032\000\033\000\
\035\000\049\000\036\000\022\000\020\000\032\000\033\000\021\000\
\000\000\000\000\034\000\000\000\000\000\050\000\000\000\036\000\
\000\000\034\000\000\000\000\000\000\000\000\000\036\000\044\000\
\044\000\044\000\044\000\044\000\000\000\044\000\044\000\044\000\
\044\000\044\000\044\000\000\000\044\000\044\000\044\000\044\000\
\000\000\044\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\000\000\000\000\000\000\078\000\056\000\057\000\058\000\
\059\000\060\000\000\000\061\000\062\000\063\000\064\000\065\000\
\066\000\000\000\067\000\068\000\000\000\000\000\000\000\096\000\
\056\000\057\000\058\000\059\000\060\000\000\000\061\000\062\000\
\063\000\064\000\065\000\066\000\000\000\067\000\068\000\030\000\
\030\000\000\000\097\000\000\000\000\000\030\000\030\000\030\000\
\030\000\030\000\030\000\000\000\030\000\030\000\030\000\030\000\
\000\000\030\000\035\000\035\000\035\000\035\000\035\000\035\000\
\000\000\035\000\035\000\035\000\035\000\000\000\035\000\031\000\
\031\000\000\000\000\000\003\000\004\000\031\000\031\000\031\000\
\031\000\031\000\031\000\000\000\031\000\031\000\031\000\031\000\
\000\000\031\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\069\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\077\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\092\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\099\000\044\000\044\000\044\000\044\000\044\000\000\000\
\044\000\044\000\044\000\044\000\044\000\044\000\000\000\044\000\
\044\000\044\000\056\000\057\000\058\000\059\000\060\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\067\000\
\068\000\056\000\057\000\058\000\059\000\060\000\000\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\067\000\034\000\
\034\000\034\000\034\000\034\000\034\000\000\000\034\000\034\000\
\034\000\034\000\000\000\034\000\032\000\032\000\032\000\032\000\
\032\000\032\000\000\000\032\000\032\000\032\000\032\000\000\000\
\032\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\033\000\033\000\033\000\033\000\000\000\033\000\037\000\037\000\
\000\000\000\000\000\000\000\000\000\000\037\000\037\000\037\000\
\037\000\000\000\037\000\056\000\057\000\058\000\059\000\060\000\
\000\000\061\000\062\000\063\000\064\000\065\000\066\000\056\000\
\057\000\058\000\059\000\060\000\000\000\000\000\000\000\063\000\
\064\000\065\000\066\000"

let yycheck = "\001\000\
\000\000\031\000\032\000\033\000\034\000\027\001\036\000\029\001\
\096\000\097\000\017\001\018\001\042\000\043\000\102\000\045\000\
\046\000\024\001\025\001\026\001\027\001\002\001\029\001\013\001\
\014\001\015\001\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\003\001\
\004\001\001\001\002\001\003\001\074\000\005\001\016\001\007\001\
\008\001\009\001\010\001\011\001\012\001\011\001\012\001\013\001\
\014\001\015\001\028\001\027\001\003\001\029\001\028\001\023\001\
\094\000\002\001\026\001\029\001\028\001\030\001\030\001\031\001\
\001\001\002\001\003\001\027\001\005\001\002\001\007\001\008\001\
\009\001\010\001\011\001\012\001\024\001\025\001\026\001\027\001\
\029\001\029\001\028\001\025\001\026\001\027\001\023\001\029\001\
\028\001\026\001\026\001\028\001\026\001\030\001\031\001\001\001\
\002\001\003\001\016\001\005\001\028\001\007\001\008\001\009\001\
\010\001\011\001\012\001\027\001\001\001\002\001\006\001\006\000\
\029\001\029\001\029\001\001\001\002\001\023\001\011\001\012\001\
\026\001\029\001\028\001\019\000\030\001\011\001\012\001\018\000\
\255\255\255\255\023\001\255\255\255\255\026\001\255\255\028\001\
\255\255\023\001\255\255\255\255\255\255\255\255\028\001\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\027\001\
\255\255\029\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\255\255\255\255\255\255\029\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\025\001\255\255\255\255\255\255\029\001\
\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\011\001\
\012\001\255\255\029\001\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\027\001\
\255\255\029\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\025\001\026\001\027\001\255\255\029\001\011\001\
\012\001\255\255\255\255\003\001\004\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\027\001\
\255\255\029\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\011\001\012\001\013\001\014\001\015\001\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\026\001\027\001\255\255\029\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\026\001\027\001\255\255\
\029\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\027\001\255\255\029\001\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\025\001\026\001\
\027\001\255\255\029\001\011\001\012\001\013\001\014\001\015\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001"

let yynames_const = "\
  INT_TYPE\000\
  VOID_TYPE\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  NOT\000\
  AND\000\
  OR\000\
  SEMICOLON\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'function_defs) in
    Obj.repr(
# 31 "lib/parser.mly"
                      ( _1 )
# 317 "lib/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_def) in
    Obj.repr(
# 34 "lib/parser.mly"
                 ( [_1] )
# 324 "lib/parser.ml"
               : 'function_defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'function_defs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'function_def) in
    Obj.repr(
# 35 "lib/parser.mly"
                               ( _1 @ [_2] )
# 332 "lib/parser.ml"
               : 'function_defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'type_spec) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 39 "lib/parser.mly"
      (
        {
          ret_type = _1;
          name = _2;
          params = _4;
          body = _6;
        }
      )
# 349 "lib/parser.ml"
               : 'function_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "lib/parser.mly"
                  ( [] )
# 355 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pparam_list) in
    Obj.repr(
# 50 "lib/parser.mly"
                   ( _1 )
# 362 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 53 "lib/parser.mly"
                            ( [_1] )
# 369 "lib/parser.ml"
               : 'pparam_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pparam_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 54 "lib/parser.mly"
                             ( _1 @ [_3] )
# 377 "lib/parser.ml"
               : 'pparam_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "lib/parser.mly"
             ( Int )
# 383 "lib/parser.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "lib/parser.mly"
              ( Void )
# 389 "lib/parser.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "lib/parser.mly"
                ( (Int, _2) )
# 396 "lib/parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 64 "lib/parser.mly"
                             ( Block _2 )
# 403 "lib/parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "lib/parser.mly"
                ( [] )
# 409 "lib/parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "lib/parser.mly"
                    ( _1 @ [_2] )
# 417 "lib/parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 71 "lib/parser.mly"
          ( _1 )
# 424 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "lib/parser.mly"
              ( EmptyStmt )
# 430 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "lib/parser.mly"
                   ( ExprStmt _1 )
# 437 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "lib/parser.mly"
                                      ( VarDecl (_2, _4) )
# 445 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "lib/parser.mly"
                             ( Assign (_1, _3) )
# 453 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "lib/parser.mly"
                                         ( If (_3, _5, Some _7) )
# 462 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "lib/parser.mly"
                                         ( If (_3, _5, None) )
# 470 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "lib/parser.mly"
                                  ( While (_3, _5) )
# 478 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "lib/parser.mly"
                    ( Break )
# 484 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "lib/parser.mly"
                       ( Continue )
# 490 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "lib/parser.mly"
                     ( Return None )
# 496 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "lib/parser.mly"
                          ( Return (Some _2) )
# 503 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "lib/parser.mly"
                    ( Binop (_1, Mul, _3) )
# 511 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "lib/parser.mly"
                    ( Binop (_1, Div, _3) )
# 519 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "lib/parser.mly"
                    ( Binop (_1, Mod, _3) )
# 527 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "lib/parser.mly"
                    ( Binop (_1, Add, _3) )
# 535 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "lib/parser.mly"
                    ( Binop (_1, Sub, _3) )
# 543 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "lib/parser.mly"
                    ( Binop (_1, Leq, _3) )
# 551 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "lib/parser.mly"
                    ( Binop (_1, Geq, _3) )
# 559 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "lib/parser.mly"
                    ( Binop (_1, Gt, _3) )
# 567 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "lib/parser.mly"
                    ( Binop (_1, Lt, _3) )
# 575 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "lib/parser.mly"
                    ( Binop (_1, Eq, _3) )
# 583 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "lib/parser.mly"
                    ( Binop (_1, Neq, _3) )
# 591 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "lib/parser.mly"
                    ( Binop (_1, And, _3) )
# 599 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "lib/parser.mly"
                    ( Binop (_1, Or, _3) )
# 607 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "lib/parser.mly"
                    ( Unop (Not, _2) )
# 614 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "lib/parser.mly"
                    ( Unop (Neg, _2) )
# 621 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "lib/parser.mly"
                    ( Unop (Pos, _2) )
# 628 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 101 "lib/parser.mly"
                   ( _1 )
# 635 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "lib/parser.mly"
         ( Var _1 )
# 642 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "lib/parser.mly"
          ( IntLit _1 )
# 649 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "lib/parser.mly"
                         ( _2 )
# 656 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg_list_opt) in
    Obj.repr(
# 107 "lib/parser.mly"
                                    ( Call (_1, _3) )
# 664 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "lib/parser.mly"
                  ( [] )
# 670 "lib/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 111 "lib/parser.mly"
                  ( _1 )
# 677 "lib/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "lib/parser.mly"
                        ( [_1] )
# 684 "lib/parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "lib/parser.mly"
                        ( _1 @ [_3] )
# 692 "lib/parser.ml"
               : 'arg_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
;;
