type typ =
  | Int
  | Void

type binop = 
  | Mul | Div | Mod       (* *, /, % *)
  | Add | Sub             (* +, - *)
  | Lt | Gt | Leq | Geq   (* <, >, <=, >= *)
  | Eq | Neq              (* ==, != *)
  | And | Or              (* &&, || *)

type unop =
  | Pos   (* + (一元正号) *)
  | Neg   (* - (一元负号) *)
  | Not   (* ! *)

type expr =
  | IntLit of int                  (* 整数字面量 *)
  | Var of string                  (* 变量引用 *)
  | Binop of expr * binop * expr   (* 二元操作 *)
  | Unop of unop * expr            (* 一元操作 *)
  | Assign of string * expr        (* 赋值语句 *)
  | Call of string * expr list     (* 函数调用 *)

type stmt =
  | EmptyStmt                     (* 空语句 ; *)
  | ExprStmt of expr              (* 表达式语句 *)
  | VarDecl of string * expr      (* 变量声明 *)
  | Assign of string * expr   (* 赋值语句 *)
  | If of expr * stmt * stmt option (* if语句 *)
  | While of expr * stmt          (* while循环 *)
  | Break                         (* break *)
  | Continue                      (* continue *)
  | Return of expr option         (* return语句 *)
  | Block of stmt list            (* 语句块 *)

type param = typ * string         (* 函数参数: 类型和名称 *)

type function_def = {
  ret_type: typ;      (* 返回类型 *)
  name: string;       (* 函数名 *)
  params: param list;  (* 参数列表 *)
  body: stmt;         (* 函数体 *)
}

type program = function_def list  (* 程序由函数定义列表组成 *)