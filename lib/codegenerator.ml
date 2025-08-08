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
  next_offset = -16;     (* 预留s1的保存空间 *)
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
let callee_saved_regs = ["s0"; "s1"]  (* 被调用者需要保存的寄存器 *)

(* 生成二元操作指令 *)
let binop_to_asm = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "rem"
  | Lt  -> "slt"
  | Gt  -> "sgt"   (* 伪指令，汇编器会处理 *)
  | _ -> failwith "Complex comparisons and logical ops need special handling"

(* 表达式代码生成 - 使用t0作为中间结果寄存器，避免覆盖a0 *)
let rec codegen_expr expr ctx =
  match expr with
  | IntLit n ->
      (["li t0, " ^ string_of_int n], "t0", ctx)  (* 结果存入t0而非a0 *)
  
  | Var name ->
      let offset = lookup_var ctx name in
      (["lw t0, " ^ string_of_int offset ^ "(s0)"], "t0", ctx)  (* 结果存入t0 *)
  
  | Binop (e1, op, e2) when op = And || op = Or ->
    (* 逻辑操作短路求值 *)
    let (code1, reg1, ctx1) = codegen_expr e1 ctx in
    let (label_end, ctx2) = new_label ctx1 "logical_end" in
    let (label_short, ctx3) = new_label ctx2 "logical_short" in
    
    (* 保存左操作数到安全寄存器 *)
    let (save_code, saved_reg) = 
        if reg1 = "t0" then (["mv t1, t0"], "t1")  (* 使用t1保存中间结果 *)
        else ([], reg1)
    in
    
    (* 根据操作符类型确定分支条件和短路值 *)
    let (branch_instr, short_value) = 
        if op = And then ("beqz", 0) else ("bnez", 1)
    in
    
    let (code2, reg2, ctx4) = codegen_expr e2 ctx3 in
    
    (* 确保结果在安全寄存器中 *)
    let (result_reg, result_code) = 
        if reg2 = "t0" then ("t2", ["mv t2, t0"])  (* 使用t2保存右操作数结果 *)
        else (reg2, [])
    in
    
    let asm = code1 @ save_code @
        [branch_instr ^ " " ^ saved_reg ^ ", " ^ label_short] @
        code2 @ result_code @
        (* 非短路分支：转换结果为布尔值 *)
        ["snez t0, " ^ result_reg] @  (* 最终结果存入t0 *)
        ["j " ^ label_end] @
        [label_short ^ ":"] @
        (* 短路分支：直接加载布尔值 *)
        ["li t0, " ^ string_of_int short_value] @  (* 最终结果存入t0 *)
        [label_end ^ ":"]
    in
    (asm, "t0", ctx4)  (* 返回t0作为结果寄存器 *)

  | Binop (e1, op, e2) ->
    let (code1, reg1, ctx1) = codegen_expr e1 ctx in
    (* 保存左操作数到保存寄存器s1 *)
    let (save_code, saved_reg) = 
        if reg1 = "s1" then ([], "s1")  (* 已在保存寄存器中 *)
        else (["mv s1, " ^ reg1], "s1")  (* 移到s1保存 *)
    in
    
    (* 保存s1到栈，避免被内层运算覆盖 *)
    let stack_offset = ctx1.next_offset in  (* 获取当前栈偏移 *)
    let save_s1_code = ["sw s1, " ^ string_of_int stack_offset ^ "(s0)"] in  (* 保存s1到栈 *)
    let new_ctx = { ctx1 with next_offset = stack_offset - 4 } in  (* 栈偏移减4（预留4字节） *)
    
    let (code2, reg2, ctx2) = codegen_expr e2 new_ctx in  (* 处理右操作数（可能嵌套运算） *)
    
    (* 从栈恢复s1的值 *)
    let restore_s1_code = ["lw s1, " ^ string_of_int stack_offset ^ "(s0)"] in  (* 恢复s1 *)
    
    (* 右操作数处理 *)
    let (right_reg, right_code) = 
        if reg2 = "t0" then ("t1", ["mv t1, t0"])  (* 使用t1保存右操作数结果 *)
        else (reg2, [])
    in
    
    (* 计算指令 - 结果存入t0 *)
    let compute_instr = 
        match op with
        | Eq ->  [ "xor t2, " ^ saved_reg ^ ", " ^ right_reg; "seqz t0, t2" ]
        | Neq -> [ "xor t2, " ^ saved_reg ^ ", " ^ right_reg; "snez t0, t2" ]
        | Leq -> [ "slt t2, " ^ right_reg ^ ", " ^ saved_reg; "seqz t0, t2" ]
        | Geq -> [ "slt t2, " ^ saved_reg ^ ", " ^ right_reg; "seqz t0, t2" ]
        | _ ->   [ binop_to_asm op ^ " t0, " ^ saved_reg ^ ", " ^ right_reg ]
    in
    (* 组合代码：左操作数处理 + 保存s1 + 右操作数处理 + 恢复s1 + 计算 *)
    (code1 @ save_code @ save_s1_code @ code2 @ restore_s1_code @ right_code @ compute_instr, "t0", ctx2)
  
  | Unop (Pos, e) ->  
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code, reg, ctx1)  (* 正号不需要生成额外指令 *)
  
  | Unop (Neg, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["neg " ^ reg ^ ", " ^ reg], reg, ctx1)
  
  | Unop (Not, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["seqz " ^ reg ^ ", " ^ reg], reg, ctx1)
  
  | Assign (name, e) ->
      let offset = lookup_var ctx name in
      let (code, reg, ctx1) = codegen_expr e ctx in
      (* 将t0中的值存入变量地址 *)
      (code @ ["sw " ^ reg ^ ", " ^  string_of_int offset ^ "(s0)"], reg, ctx1)
  
  | Call (func, args) ->
      (* 计算栈参数数量 *)
      let num_reg_args = min (List.length args) (List.length arg_regs) in
      let num_stack_args = List.length args - num_reg_args in
      let stack_space = num_stack_args * 4 in
      
      (* 生成参数：前8个用寄存器，剩余用栈 *)
      let rec gen_args args ctx index =
        match args with
        | [] -> ([], ctx)
        | arg::rest ->
            let (code, reg, ctx1) = codegen_expr arg ctx in
            let arg_code, ctx2 = 
              if index < List.length arg_regs then (  (* 寄存器参数 *)
                let target_reg = List.nth arg_regs index in
                let optimized_code = 
                  match arg with
                  | IntLit n -> ["li " ^ target_reg ^ ", " ^ string_of_int n]
                  | _ -> if reg = target_reg then code else code @ ["mv " ^ target_reg ^ ", " ^ reg]
                in
                (optimized_code, ctx1)
              ) else (  (* 栈参数 *)
                let stack_offset = -16 - (index * 4) in  (* 统一偏移计算，避免冲突 *)
                (code @ [ "sw " ^ reg ^ ", " ^ string_of_int stack_offset ^ "(s0)" ], ctx1)
              )
            in
            let (rest_code, ctx3) = gen_args rest ctx2 (index + 1) in
            (arg_code @ rest_code, ctx3)
          in
          
      (* 传递栈参数（如果有） *)
      let push_stack_args = 
        if num_stack_args > 0 then
          [ "addi sp, sp, " ^ string_of_int (-stack_space) ] @
          (List.init num_stack_args (fun i ->
            let src_offset = -16 - (num_reg_args + i) * 4 in  (* 栈参数在栈帧中的源偏移 *)
            [ "lw t0, " ^ string_of_int src_offset ^ "(s0)";
              "sw t0, " ^ string_of_int (i * 4) ^ "(sp)" ]  (* 写入调用栈 *)
          ) |> List.flatten)
        else []
      in
      
      let (arg_code, ctx1) = gen_args args ctx 0 in
      let call_code = [ "call " ^ func ] in
      let cleanup_stack = if stack_space > 0 then [ "addi sp, sp, " ^ string_of_int stack_space ] else [] in
      
      (arg_code @ push_stack_args @ call_code @ cleanup_stack @ ["mv t0, a0"], "t0", ctx1)  (* 结果存入t0 *)



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
      
      (* 条件判断使用t0中的结果 *)
      let cond_jump = ["beqz " ^ cond_reg ^ ", " ^ label_else] in
      
      let (then_code, ctx4) = codegen_stmt then_stmt ctx3 in
      let (else_code, ctx5) = match else_stmt with
        | Some s -> codegen_stmt s ctx4
        | None -> ([], ctx4)
      in
      
      let asm = cond_code @ cond_jump @
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
        ["bnez " ^ cond_reg ^ ", " ^ label_start] @  (* 使用t0中的条件结果 *)
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
      (* 函数返回值需存入a0 *)
      (code @ ["mv a0, " ^ reg; "j " ^ ctx.current_function ^ "_exit"], ctx1)
  
  | Block stmts ->
      (* 保存块开始前的环境（只恢复环境，不恢复栈偏移） *)
      let saved_env = ctx.env in
      
      (* 生成块内代码，允许标签计数器和栈偏移递增 *)
      let rec gen_block stmts current_ctx =
        match stmts with
        | [] -> ([], current_ctx)
        | stmt::rest ->
            let (code1, ctx1) = codegen_stmt stmt current_ctx in
            let (code2, ctx2) = gen_block rest ctx1 in
            (code1 @ code2, ctx2)
      in
      let (block_code, ctx_after_block) = gen_block stmts ctx in
      
      (* 仅恢复环境，保留更新后的栈偏移和标签计数器 *)
      let restored_ctx = {
        ctx_after_block with
        env = saved_env;
      } in
      
      (block_code, restored_ctx)  (* 关键修复：不恢复next_offset，保留栈偏移累计 *)

(* 函数代码生成 *)
let codegen_function func =
  let ctx = initial_context func.name in
  
  (* 分配参数空间 - 修正元组处理，正确提取context *)
  let (_, ctx_with_params) =  (* 只需要元组中的第二个元素（context） *)
    List.fold_left
      (fun (i, ctx) (typ, name) ->
        let size = size_of typ in
        (* 前8个参数偏移：-16 - i*4（对应a0-a7保存到栈帧），超过8个继续累加偏移 *)
        let offset = -16 - i * size in  
        let next_ctx = { 
          ctx with 
          env = (name, offset) :: ctx.env;
          next_offset = min ctx.next_offset (offset - size)
        } in
        (i + 1, next_ctx))
      (0, ctx)
      func.params
  in
  
  (* 生成参数寄存器保存指令：将a0/a1等寄存器参数保存到栈帧 *)
  let param_save_code =
    List.mapi (fun i (_, name) ->
      let reg = if i < List.length arg_regs then List.nth arg_regs i else "" in
      let offset = lookup_var ctx_with_params name in  (* 现在ctx_with_params是正确的context类型 *)
      if i < List.length arg_regs then
        "sw " ^ reg ^ ", " ^ string_of_int offset ^ "(s0)"  (* 保存寄存器到栈 *)
      else
        ""  (* 栈参数不需要从寄存器保存 *)
    ) func.params
    |> List.filter (fun s -> s <> "")  (* 过滤空字符串 *)
  in
  
  (* 生成函数体 *)
  let (body_code, ctx_body) = codegen_stmt func.body ctx_with_params in
  
  (* 计算栈帧大小（确保足够保存所有变量和被调用者寄存器） *)
  let frame_size = 
    let min_offset = ctx_body.next_offset in
    let total = (-min_offset + 15) land (lnot 15) in  (* 按16字节对齐 *)
    max total (32 + (List.length callee_saved_regs - 2) * 4)  (* 基础空间需求 *)
  in
  
  (* 函数序言：保存返回地址、栈帧基址和被调用者保存寄存器 *)
  let prologue = [
    func.name ^ ":";
    "addi sp, sp, -" ^ string_of_int frame_size;
    "sw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";    (* 保存返回地址 *)
    "sw s0, " ^ string_of_int (frame_size - 8) ^ "(sp)";    (* 保存栈帧基址s0 *)
    "sw s1, " ^ string_of_int (frame_size - 12) ^ "(sp)";   (* 保存s1 *)
    "addi s0, sp, " ^ string_of_int frame_size;             (* 设置栈帧基址 *)
  ] @ param_save_code  (* 追加参数寄存器保存指令 *)
  in
  
  (* 函数结语：恢复被调用者保存寄存器、返回地址和栈帧基址 *)
  let epilogue = [
    func.name ^ "_exit:";
    "lw s1, " ^ string_of_int (frame_size - 12) ^ "(sp)";   (* 恢复s1 *)
    "lw s0, " ^ string_of_int (frame_size - 8) ^ "(sp)";    (* 恢复栈帧基址s0 *)
    "lw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";    (* 恢复返回地址 *)
    "addi sp, sp, " ^ string_of_int frame_size;
    "ret"
  ] in
  
  (prologue @ body_code @ epilogue)

(* 程序入口点生成 *)
let codegen_program (program : program) =
  (* 确保 main 函数被标记为全局 *)
  let main_decl = 
    if List.exists (fun f -> f.name = "main") program then
      [".global main"]
    else 
      [] 
  in
  
  (* 生成所有函数代码 *)
  let funcs_code = List.map codegen_function program in
  
  (* 添加系统调用退出指令 *)
  let exit_code = 
    if List.exists (fun f -> f.name = "main") program then
      ["li a7, 93"; "ecall"]  (* 仅在存在 main 时添加退出 *)
    else 
      [] 
  in
  
  (* 组合所有部分 *)
  main_decl @ (List.flatten funcs_code) @ exit_code

(* 辅助函数 *)
let string_of_offset n = 
  if n >= 0 then "+" ^ string_of_int n else string_of_int n
    