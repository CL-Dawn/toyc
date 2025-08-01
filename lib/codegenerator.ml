open Ast

(* 环境管理：变量名 -> 栈偏移量 *)
type env = (string * int) list

(* 代码生成上下文 *)
type context = {
  env : env;             (* 当前变量环境 *)
  next_offset : int;     (* 栈帧中下一个可用偏移量 *)
  label_counter : int;   (* 标签计数器 *)
  current_function : string; (* 当前函数名 *)
  loop_stack : (string * string) list; (* 循环标签栈 (break_label, continue_label) *)
}

(* 初始化上下文 *)
let initial_context func_name = {
  env = [];
  next_offset = -12;     (* 初始偏移: ra(4) + fp(4) + 预留(4) = 12字节 *)
  label_counter = 0;
  current_function = func_name;
  loop_stack = [];
}

(* 生成唯一标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s_%s_%d" ctx.current_function prefix ctx.label_counter in
  (label, { ctx with label_counter = ctx.label_counter + 1 })

(* 在栈上分配空间并更新环境 *)
let alloc_var ctx name size =
  let offset = ctx.next_offset in
  let new_ctx = {
    ctx with
    env = (name, offset) :: ctx.env;
    next_offset = offset - size;
  } in
  (offset, new_ctx)

(* 查找变量偏移量 *)
let lookup_var ctx name =
  try List.assoc name ctx.env
  with Not_found -> failwith ("Undefined variable: " ^ name)

(* 类型大小计算 *)
let size_of = function
  | Int -> 4
  | Void -> 0

(* 寄存器分配策略 *)
let arg_regs = ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]
let tmp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"]

(* 生成二元操作指令 *)
let binop_to_asm = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "rem"
  | Lt  -> "slt"
  | Gt  -> "sgt"
  | Leq -> "sle"
  | Geq -> "sge"
  | Eq  -> "seqz"  (* 需要特殊处理 *)
  | Neq -> "snez" (* 需要特殊处理 *)
  | And | Or -> failwith "Logical ops need special handling"

(* 表达式代码生成 *)
let rec codegen_expr expr ctx =
  match expr with
  | IntLit n ->
      (["li a0, " ^ string_of_int n], "a0", ctx)
  
  | Var name ->
      let offset = lookup_var ctx name in
      (["lw a0, " ^ string_of_int offset ^ "(s0)"], "a0", ctx)
  
  | Binop (e1, op, e2) when op = And || op = Or ->
      (* 逻辑操作短路求值 *)
      let (code1, reg1, ctx1) = codegen_expr e1 ctx in
      let (label_end, ctx2) = new_label ctx1 "logical_end" in
      let (label_false, ctx3) = new_label ctx2 "logical_false" in
      
      let branch_instr = if op = And then "beqz" else "bnez" in
      let set_instr = if op = And then "li" else "mv" in
      
      let code2, reg2, ctx4 = codegen_expr e2 ctx3 in
      
      let asm = code1 @
        [branch_instr ^ " " ^ reg1 ^ ", " ^ label_false] @
        code2 @
        ["j " ^ label_end] @
        [label_false ^ ":"] @
        [set_instr ^ " " ^ reg2 ^ ", " ^ reg1] @
        [label_end ^ ":"]
      in
      (asm, reg2, ctx4)

  | Binop (e1, op, e2) ->
      let (code1, reg1, ctx1) = codegen_expr e1 ctx in
      let (code2, reg2, ctx2) = codegen_expr e2 ctx1 in
      (* 使用临时寄存器存储第二个操作数 *)
      let tmp = List.hd tmp_regs in
      
      let asm = match op with
        | Eq -> 
            code1 @ code2 @ 
            ["addi " ^ tmp ^ ", " ^ reg2 ^ ", 0"] @  
            ["sub " ^ tmp ^ ", " ^ reg1 ^ ", " ^ tmp;
             "seqz a0, " ^ tmp]
        | Neq ->
            code1 @ code2 @ 
            ["addi " ^ tmp ^ ", " ^ reg2 ^ ", 0"] @  
            ["sub " ^ tmp ^ ", " ^ reg1 ^ ", " ^ tmp;
             "snez a0, " ^ tmp]
        | _ ->
            let instr = binop_to_asm op in
            code1 @ 
            ["addi " ^ tmp ^ ", " ^ reg1 ^ ", 0"] @  
            code2 @ 
            [instr ^ " a0, " ^ tmp ^ ", " ^ reg2]
      in
      (asm, "a0", ctx2)
  | Unop (Pos, e) ->  
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code, reg, ctx1)  (* 正号不需要生成额外指令 *)
  | Unop (Neg, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["neg a0, " ^ reg], "a0", ctx1)
  
  | Unop (Not, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["seqz a0, " ^ reg], "a0", ctx1)
  
  | Assign (name, e) ->
      let offset = lookup_var ctx name in
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["sw " ^ reg ^ ", " ^  string_of_int offset ^ "(s0)"], reg, ctx1)
  
  | Call (func, args) ->
  (* 计算栈参数数量 *)
  let num_stack = max 0 (List.length args - List.length arg_regs) in
  let stack_space = num_stack * 4 in
  
  (* 分配栈空间 *)
  let adjust_sp = 
    if stack_space > 0 then 
      ["addi sp, sp, " ^ string_of_int (-stack_space)] 
    else [] 
  in

  (* 生成参数代码 *)
  let rec gen_args args ctx regs stack_index =
    match args, regs with
    | [], _ -> ([], ctx)
    | arg::rest, r::rs ->
        let (code, reg_arg, ctx1) = codegen_expr arg ctx in
        let (rest_code, ctx2) = gen_args rest ctx1 rs stack_index in
        (code @ ["addi " ^ r ^ ", " ^ reg_arg ^ ", 0"] @ rest_code, ctx2)
    | arg::rest, [] ->
        let (code, reg_arg, ctx1) = codegen_expr arg ctx in
        let offset = stack_index * 4 in
        let (rest_code, ctx2) = gen_args rest ctx1 [] (stack_index + 1) in
        (code @ ["sw " ^ reg_arg ^ ", " ^ string_of_int offset ^ "(sp)"] 
         @ rest_code, ctx2)
  in

  let (arg_code, ctx1) = gen_args args ctx arg_regs 0 in
  let call_code = ["call " ^ func] in
  
  (* 释放栈空间 *)
  let restore_sp = 
    if stack_space > 0 then 
      ["addi sp, sp, " ^ string_of_int stack_space] 
    else [] 
  in

  (adjust_sp @ arg_code @ call_code @ restore_sp, "a0", ctx1)

(* 语句代码生成 *)
let rec codegen_stmt stmt ctx =
  match stmt with
  | EmptyStmt -> ([], ctx)
  
  | ExprStmt e ->
      let (code, _, ctx1) = codegen_expr e ctx in
      (code, ctx1)
  
  | VarDecl (name, e) ->
      let (offset, ctx1) = alloc_var ctx name 4 in
      let (code, reg, ctx2) = codegen_expr e ctx1 in
      (code @ ["sw " ^ reg ^ ", " ^  string_of_int offset ^ "(s0)"], ctx2)
  
  | Assign (name, e) ->
      let (code, _, ctx1) = codegen_expr (Assign (name, e)) ctx in
      (code, ctx1)
  
  | If (cond, then_stmt, else_stmt) ->
      let (cond_code, cond_reg, ctx1) = codegen_expr cond ctx in
      let (label_else, ctx2) = new_label ctx1 "if_else" in
      let (label_end, ctx3) = new_label ctx2 "if_end" in
      
      let (then_code, ctx4) = codegen_stmt then_stmt ctx3 in
      let (else_code, ctx5) = match else_stmt with
        | Some s -> codegen_stmt s ctx4
        | None -> ([], ctx4)
      in
      
      let asm = cond_code @
        ["beqz " ^ cond_reg ^ ", " ^ label_else] @
        then_code @
        ["j " ^ label_end] @
        [label_else ^ ":"] @
        else_code @
        [label_end ^ ":"]
      in
      (asm, ctx5)
  
  | While (cond, body) ->
      let (label_start, ctx1) = new_label ctx "loop_start" in
      let (label_cont, ctx2) = new_label ctx1 "loop_continue" in
      let (label_end, ctx3) = new_label ctx2 "loop_end" in
      
      let ctx4 = { ctx3 with loop_stack = (label_end, label_cont) :: ctx3.loop_stack } in
      let (cond_code, cond_reg, ctx5) = codegen_expr cond ctx4 in
      let (body_code, ctx6) = codegen_stmt body ctx5 in
      let ctx7 = { ctx6 with loop_stack = List.tl ctx6.loop_stack } in
      
      let asm =
        ["j " ^ label_cont] @
        [label_start ^ ":"] @
        body_code @
        [label_cont ^ ":"] @
        cond_code @
        ["bnez " ^ cond_reg ^ ", " ^ label_start] @
        [label_end ^ ":"]
      in
      (asm, ctx7)
  
  | Break ->
      (match ctx.loop_stack with
       | (break_label, _) :: _ -> (["j " ^ break_label], ctx)
       | [] -> failwith "Break outside loop")
  
  | Continue ->
      (match ctx.loop_stack with
       | (_, cont_label) :: _ -> (["j " ^ cont_label], ctx)
       | [] -> failwith "Continue outside loop")
  
  | Return None ->
      (["j " ^ ctx.current_function ^ "_exit"], ctx)
  
  | Return (Some e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["addi a0, " ^ reg ^ ", 0"; "j " ^ ctx.current_function ^ "_exit"], ctx1)
  
  | Block stmts ->
      let rec gen_block stmts ctx =
        match stmts with
        | [] -> ([], ctx)
        | stmt::rest ->
            let (code1, ctx1) = codegen_stmt stmt ctx in
            let (code2, ctx2) = gen_block rest ctx1 in
            (code1 @ code2, ctx2)
      in
      gen_block stmts ctx

(* 函数代码生成 *)
let codegen_function func =
  let ctx = initial_context func.name in
  
  (* 分配参数空间 *)
  let ctx_with_params =
    let (_, ctx_after_params) = 
      List.fold_left
        (fun (i, ctx) (typ, name) ->
          let size = size_of typ in
          let offset = -12 - i * size in
          let next_ctx = { 
            ctx with 
            env = (name, offset) :: ctx.env;
            next_offset = min ctx.next_offset (offset - size)
          } in
          (i + 1, next_ctx))
        (0, ctx)
        func.params
    in
    ctx_after_params
  in
  
  (* 生成函数体 *)
  let (body_code, ctx_body) = codegen_stmt func.body ctx_with_params in
  
  (* 计算栈帧大小 (16字节对齐) *)
  let frame_size = 
    let min_offset = ctx_body.next_offset in
    let total = (-min_offset + 15) land (lnot 15) in
    max total 16
  in
  
  (* 生成函数序言和结语 *)
  let prologue = [
    func.name ^ ":";
    "addi sp, sp, -" ^ string_of_int frame_size;
    "sw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";
    "sw s0, " ^ string_of_int (frame_size - 8) ^ "(sp)";
    "addi s0, sp, " ^ string_of_int frame_size
  ] in
  
  let epilogue = [
    func.name ^ "_exit:";
    "lw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";
    "lw s0, " ^ string_of_int (frame_size - 8) ^ "(sp)";
    "addi sp, sp, " ^ string_of_int frame_size;
    "ret"
  ] in
  
  (prologue @ body_code @ epilogue)

(* 程序入口点生成 *)
let codegen_program (program : program) =
  let funcs_code = List.map codegen_function program in
  
  let main_code = if List.exists (fun f -> f.name = "main") program then
      [".global main"] 
    else [] in
  
  let all_code = List.flatten funcs_code in
  
  main_code @ all_code @ ["li a7, 93"; "ecall"]  (* 退出系统调用 *)

(* 辅助函数 *)
let string_of_offset n = 
  if n >= 0 then "+" ^ string_of_int n else string_of_int n