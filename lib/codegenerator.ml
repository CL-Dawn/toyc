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
  next_offset = -16;     (* 预留s0-s1的保存空间 (-16: s0, -20: s1) *)
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
let callee_saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]

(* 生成二元操作指令 *)
let binop_to_asm = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "rem"
  | Lt  -> "slt"
  | Gt  -> "sgt"
  | Eq  -> "seq"
  | Neq -> "sne"
  | Leq -> "sle"
  | Geq -> "sge"
  | And -> "and"
  | Or  -> "or"

(* 表达式代码生成 *)
let rec codegen_expr expr ctx =
  match expr with
  | IntLit n ->
      (["li t0, " ^ string_of_int n], "t0", ctx)
  
  | Var name ->
      let offset = lookup_var ctx name in
      (["lw t0, " ^ string_of_int offset ^ "(s0)"], "t0", ctx)
  
  | Binop (e1, op, e2) when op = And || op = Or ->
    let (code1, reg1, ctx1) = codegen_expr e1 ctx in
    let (label_end, ctx2) = new_label ctx1 "logical_end" in
    let (label_short, ctx3) = new_label ctx2 "logical_short" in
    
    let (save_code, saved_reg) = 
        if reg1 = "t0" then (["mv t1, t0"], "t1")
        else ([], reg1)
    in
    
    let (branch_instr, short_value) = 
        if op = And then ("beqz", 0) else ("bnez", 1)
    in
    
    let (code2, reg2, ctx4) = codegen_expr e2 ctx3 in
    
    let (result_reg, result_code) = 
        if reg2 = "t0" then ("t2", ["mv t2, t0"])
        else (reg2, [])
    in
    
    let asm = code1 @ save_code @
        [branch_instr ^ " " ^ saved_reg ^ ", " ^ label_short] @
        code2 @ result_code @
        ["snez t0, " ^ result_reg] @
        ["j " ^ label_end] @
        [label_short ^ ":"] @
        ["li t0, " ^ string_of_int short_value] @
        [label_end ^ ":"]
    in
    (asm, "t0", ctx4)

  | Binop (e1, op, e2) ->
    let (code1, reg1, ctx1) = codegen_expr e1 ctx in
    let (save_code, saved_reg) = 
        if reg1 = "s1" then ([], "s1")
        else (["mv s1, " ^ reg1], "s1")
    in
    
    let stack_offset = ctx1.next_offset in
    let save_s1_code = ["sw s1, " ^ string_of_int stack_offset ^ "(s0)"] in
    let new_ctx = { ctx1 with next_offset = stack_offset - 4 } in
    
    let (code2, reg2, ctx2) = codegen_expr e2 new_ctx in
    
    let restore_s1_code = ["lw s1, " ^ string_of_int stack_offset ^ "(s0)"] in
    
    let (right_reg, right_code) = 
        if reg2 = "t0" then ("t1", ["mv t1, t0"])
        else (reg2, [])
    in
    
    let compute_instr = 
        match op with
        | And -> ["and t0, " ^ saved_reg ^ ", " ^ right_reg; "snez t0, t0"]
        | Or  -> ["or t0, " ^ saved_reg ^ ", " ^ right_reg; "snez t0, t0"]
        | _ -> [binop_to_asm op ^ " t0, " ^ saved_reg ^ ", " ^ right_reg]
    in
    
    (code1 @ save_code @ save_s1_code @ code2 @ restore_s1_code @ right_code @ compute_instr, "t0", ctx2)
  
  | Unop (Pos, e) ->  
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code, reg, ctx1)
  
  | Unop (Neg, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["neg " ^ reg ^ ", " ^ reg], reg, ctx1)
  
  | Unop (Not, e) ->
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["seqz " ^ reg ^ ", " ^ reg], reg, ctx1)
  
  | Assign (name, e) ->
      let offset = lookup_var ctx name in
      let (code, reg, ctx1) = codegen_expr e ctx in
      (code @ ["sw " ^ reg ^ ", " ^ string_of_int offset ^ "(s0)"], reg, ctx1)
  
  | Call (func, args) ->
      let num_reg_args = min (List.length args) (List.length arg_regs) in
      let num_stack_args = List.length args - num_reg_args in
      let stack_space = num_stack_args * 4 in
      
      let rec gen_args args ctx index =
        match args with
        | [] -> ([], ctx)
        | arg::rest ->
            let (code, reg, ctx1) = codegen_expr arg ctx in
            let arg_code, ctx2 = 
              if index < List.length arg_regs then (
                let target_reg = List.nth arg_regs index in
                let optimized_code = 
                  match arg with
                  | IntLit n -> ["li " ^ target_reg ^ ", " ^ string_of_int n]
                  | _ -> if reg = target_reg then code else code @ ["mv " ^ target_reg ^ ", " ^ reg]
                in
                (optimized_code, ctx1)
              ) else (
                let stack_offset = ctx1.next_offset in
                let store_code = code @ ["sw " ^ reg ^ ", " ^ string_of_int stack_offset ^ "(s0)"] in
                (store_code, { ctx1 with next_offset = stack_offset - 4 })
              )
            in
            let (rest_code, ctx3) = gen_args rest ctx2 (index + 1) in
            (arg_code @ rest_code, ctx3)
          in
          
      (* 修复栈参数传递逻辑 - 正确处理List.mapi的返回值 *)
      let push_stack_args = 
        if num_stack_args > 0 then
          let stack_args = 
            List.init num_stack_args (fun i ->
              let src_offset = -16 - (num_reg_args + i) * 4 - 4 in
              [ "lw t0, " ^ string_of_int src_offset ^ "(s0)";
                "sw t0, " ^ string_of_int (i * 4) ^ "(sp)" ])
            |> List.flatten  (* 将列表的列表扁平化为单一列表 *)
          in
          [ "addi sp, sp, " ^ string_of_int (-stack_space) ] @ stack_args
        else []
      in
      
      let (arg_code, ctx1) = gen_args args ctx 0 in
      let call_code = [ "call " ^ func ] in
      let cleanup_stack = if stack_space > 0 then [ "addi sp, sp, " ^ string_of_int stack_space ] else [] in
      
      (arg_code @ push_stack_args @ call_code @ cleanup_stack @ ["mv t0, a0"], "t0", ctx1)

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
      (code @ ["sw " ^ reg ^ ", " ^ string_of_int offset ^ "(s0)"], ctx2)
  
  | Assign (name, e) ->
      let (code, _, ctx1) = codegen_expr (Assign (name, e)) ctx in
      (code, ctx1)
  
  | If (cond, then_stmt, else_stmt) ->
      let (cond_code, cond_reg, ctx1) = codegen_expr cond ctx in
      let (label_else, ctx2) = new_label ctx1 "if_else" in
      let (label_end, ctx3) = new_label ctx2 "if_end" in
      
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
      (code @ ["mv a0, " ^ reg; "j " ^ ctx.current_function ^ "_exit"], ctx1)
  
  | Block stmts ->
      let saved_env = ctx.env in
      
      let rec gen_block stmts current_ctx =
        match stmts with
        | [] -> ([], current_ctx)
        | stmt::rest ->
            let (code1, ctx1) = codegen_stmt stmt current_ctx in
            let (code2, ctx2) = gen_block rest ctx1 in
            (code1 @ code2, ctx2)
      in
      let (block_code, ctx_after_block) = gen_block stmts ctx in
      
      let restored_ctx = {
        ctx_after_block with
        env = saved_env;
      } in
      
      (block_code, restored_ctx)

(* 函数代码生成 *)
let codegen_function func =
  let ctx = initial_context func.name in
  
  let (_, ctx_with_params) =
    List.fold_left
      (fun (i, ctx) (typ, name) ->
        let size = size_of typ in
        let offset = if i < 8 then -16 - i * size else -48 - (i - 8) * size in
        let next_ctx = { 
          ctx with 
          env = (name, offset) :: ctx.env;
          next_offset = min ctx.next_offset (offset - size)
        } in
        (i + 1, next_ctx))
      (0, ctx)
      func.params
  in
  
  let param_save_code =
    List.mapi (fun i (_, name) ->
      if i < List.length arg_regs then
        let reg = List.nth arg_regs i in
        let offset = lookup_var ctx_with_params name in
        "sw " ^ reg ^ ", " ^ string_of_int offset ^ "(s0)"
      else "")
      func.params
    |> List.filter (fun s -> s <> "")
  in
  
  let (body_code, ctx_body) = codegen_stmt func.body ctx_with_params in
  
  let frame_size = 
    let min_offset = ctx_body.next_offset in
    let total = (-min_offset + 15) land (lnot 15) in
    max total (4 * List.length callee_saved_regs + 4)
  in
  
  let save_callee_regs = 
    List.mapi (fun i reg ->
      let offset = frame_size - 4 * (i + 2) in
      "sw " ^ reg ^ ", " ^ string_of_int offset ^ "(sp)")
    callee_saved_regs
  in
  
  let restore_callee_regs = 
    List.mapi (fun i reg ->
      let offset = frame_size - 4 * (i + 2) in
      "lw " ^ reg ^ ", " ^ string_of_int offset ^ "(sp)")
    (List.rev callee_saved_regs)
  in
  
  let prologue = [
    func.name ^ ":";
    "addi sp, sp, -" ^ string_of_int frame_size;
    "sw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";
  ] @ save_callee_regs @ [
    "addi s0, sp, " ^ string_of_int frame_size;
  ] @ param_save_code
  in
  
  let epilogue = [
    func.name ^ "_exit:";
  ] @ restore_callee_regs @ [
    "lw ra, " ^ string_of_int (frame_size - 4) ^ "(sp)";
    "addi sp, sp, " ^ string_of_int frame_size;
    "ret"
  ] in
  
  (prologue @ body_code @ epilogue)

(* 程序入口点生成 *)
let codegen_program (program : program) =
  let main_decl = 
    if List.exists (fun f -> f.name = "main") program then
      [".global main"]
    else [] 
  in
  
  let funcs_code = List.map codegen_function program in
  
  let exit_code = 
    if List.exists (fun f -> f.name = "main") program then
      ["li a7, 93"; "ecall"]
    else [] 
  in
  
  main_decl @ (List.flatten funcs_code) @ exit_code

let string_of_offset n = 
  if n >= 0 then "+" ^ string_of_int n else string_of_int n
