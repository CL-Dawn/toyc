(* semant.ml *)
open Ast

(* 符号表相关定义 *)
type symbol = 
  | Variable of typ
  | Function of typ * typ list (* 返回类型 * 参数类型列表 *)

type scope = {
  parent: scope option;
  symbols: (string, symbol) Hashtbl.t;
}

type context = {
  current_scope: scope;
  in_loop: bool;
  current_return_type: typ option;
  functions: function_def list;
  errors: string list ref;
}

(* 错误报告辅助函数 *)
let error ctx msg = 
  ctx.errors := msg :: !(ctx.errors)

(* 创建新作用域 *)
let enter_scope ctx =
  { ctx with 
    current_scope = { 
      parent = Some ctx.current_scope; 
      symbols = Hashtbl.create 16 
    } 
  }

let leave_scope ctx =
  match ctx.current_scope.parent with
  | Some parent -> { ctx with current_scope = parent }
  | None -> ctx (* 不应该离开全局作用域 *)

(* 查找符号 *)
let lookup_symbol ctx name =
  let rec lookup_in_scope scope =
    match Hashtbl.find_opt scope.symbols name with
    | Some sym -> Some sym
    | None -> match scope.parent with
              | Some parent -> lookup_in_scope parent
              | None -> None
  in
  lookup_in_scope ctx.current_scope

(* 添加符号到当前作用域 *)
let add_symbol ctx name sym =
  if Hashtbl.mem ctx.current_scope.symbols name then
    error ctx (Printf.sprintf "重复定义的标识符: %s" name)
  else
    Hashtbl.add ctx.current_scope.symbols name sym

(* 检查程序语义 *)
let rec check_program (program: program) =
  let errors = ref [] in
  let ctx = {
    current_scope = { parent = None; symbols = Hashtbl.create 16 };
    in_loop = false;
    current_return_type = None;
    functions = program;
    errors = errors;
  } in
  
  (* 首先收集所有函数定义 *)
  List.iter (fun func ->
    match lookup_symbol ctx func.name with
    | Some _ -> error ctx (Printf.sprintf "重复定义的函数: %s" func.name)
    | None -> add_symbol ctx func.name (Function (func.ret_type, 
                          List.map (fun (t, _) -> t) func.params))
  ) program;
  
  (* 检查必须有一个main函数 *)
  (match lookup_symbol ctx "main" with
| Some (Function (Int, [])) -> ()  (* 正确形式 *)
| Some (Function _) -> error ctx "main函数必须有int返回类型且无参数"
| Some (Variable _) -> error ctx "main必须是函数，不能是变量"  (* 新增 *)
| None -> error ctx "程序必须包含main函数");
  
  (* 检查每个函数的语义 *)
  List.iter (check_function ctx) program;
  
  if !errors <> [] then
    List.iter (Printf.eprintf "语义错误: %s\n") (List.rev !errors);
  !errors = []

(* 检查函数语义 *)
and check_function ctx func =
  let ctx = { 
    (enter_scope ctx) with 
    current_return_type = Some func.ret_type;
  } in
  
  (* 添加参数到作用域 *)
  List.iter (fun (typ, name) ->
    add_symbol ctx name (Variable typ)
  ) func.params;
  
  check_stmt ctx func.body;
  
  (* 检查返回值 *)
  if func.ret_type = Int then
    if not (has_return func.body) then
      error ctx (Printf.sprintf "函数%s缺少return语句" func.name)

(* 检查语句是否有return *)
and has_return = function
  | Return _ -> true
  | If (_, then_stmt, else_stmt) ->
      has_return then_stmt && 
      (match else_stmt with Some s -> has_return s | None -> false)
  | Block stmts ->
      List.exists has_return stmts
  | _ -> false

(* 检查语句语义 *)
and check_stmt ctx = function
  | EmptyStmt -> ()
  | ExprStmt expr -> ignore (check_expr ctx expr)
  | VarDecl (name, expr) ->
      let expr_type = check_expr ctx expr in
      if expr_type <> Int then
        error ctx "变量初始化表达式必须是int类型";
      add_symbol ctx name (Variable Int)
  | Assign (name, expr) ->
      (match lookup_symbol ctx name with
      | Some (Variable _) ->
          let expr_type = check_expr ctx expr in
          if expr_type <> Int then
            error ctx "赋值表达式必须是int类型"
      | _ -> error ctx (Printf.sprintf "未定义的变量: %s" name))
  | If (cond, then_stmt, else_stmt) ->
      let cond_type = check_expr ctx cond in
      if cond_type <> Int then
        error ctx "if条件必须是int类型";
      check_stmt ctx then_stmt;
      Option.iter (check_stmt ctx) else_stmt
  | While (cond, body) ->
      let cond_type = check_expr ctx cond in
      if cond_type <> Int then
        error ctx "while条件必须是int类型";
      let ctx = { ctx with in_loop = true } in
      check_stmt ctx body
  | Break | Continue ->
      if not ctx.in_loop then
        error ctx "break/continue只能在循环中使用"
  | Return expr_opt ->
      (match expr_opt, ctx.current_return_type with
      | Some expr, Some Int ->
          let expr_type = check_expr ctx expr in
          if expr_type <> Int then
            error ctx "return表达式必须是int类型"
      | None, Some Void -> ()
      | Some _, Some Void ->
          error ctx "void函数不能返回非void值"
      | None, Some Int ->
          error ctx "int函数必须返回int值"
      | _, None -> assert false (* 不应该发生 *))
  | Block stmts ->
      let ctx = enter_scope ctx in
      List.iter (check_stmt ctx) stmts;
      ignore (leave_scope ctx)

(* 检查表达式语义 *)
and check_expr ctx = function
  | IntLit _ -> Int
  | Var name ->
      (match lookup_symbol ctx name with
      | Some (Variable typ) -> typ
      | _ -> error ctx (Printf.sprintf "未定义的变量: %s" name);
      Int)
  | Binop (left, _, right) ->
      let lt = check_expr ctx left in
      let rt = check_expr ctx right in
      if lt <> Int || rt <> Int then (
        error ctx "二元操作的操作数必须是int类型";
        Int
      ) else Int
  | Unop (_, expr) ->
      let t = check_expr ctx expr in
      if t <> Int then (
        error ctx "一元操作的操作数必须是int类型";
        Int
      ) else Int
  | Assign (name, expr) ->
      (match lookup_symbol ctx name with
      | Some (Variable _) ->
          let expr_type = check_expr ctx expr in
          if expr_type <> Int then
            error ctx "赋值表达式必须是int类型";
          Int
      | _ -> 
          error ctx (Printf.sprintf "未定义的变量: %s" name);
          Int)
  | Call (name, args) ->
      (match lookup_symbol ctx name with
      | Some (Function (ret_typ, param_types)) ->
          if List.length args <> List.length param_types then
            error ctx (Printf.sprintf "函数%s参数数量不匹配" name)
          else
            List.iter2 (fun arg expected ->
              let actual = check_expr ctx arg in
              if actual <> expected then
                error ctx (Printf.sprintf "函数%s参数类型不匹配" name)
            ) args param_types;
          ret_typ
      | _ -> 
          error ctx (Printf.sprintf "未定义的函数: %s" name);
          Int)