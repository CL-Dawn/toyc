(* bin/main.ml *)
open Toyc
open Lexing

(* 打印错误位置的工具函数 *)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.eprintf "Error at line %d, column %d:\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

(* 主解析和检查流程 *)
let process_source code =
  let lexbuf = Lexing.from_string code in
  (* 设置文件名用于错误报告 *)
  lexbuf.lex_curr_p <- { 
    lexbuf.lex_curr_p with 
    pos_fname = "<input>" 
  };
  
  try
    (* 1. 词法分析 -> 语法分析 *)
    let ast = Parser.program Lexer.token lexbuf in
    
    (* 打印AST结构 *)
    Printf.printf "\n[DEBUG] Generated AST:\n";
    (* 需要先在ast.ml中实现pp_program函数 *)
    (* Printf.printf "%a\n" Ast.pp_program ast; *) 
    
    (* 2. 语义分析 *)
    if Semant.check_program ast then begin
      (*Printf.printf "Semantic check passed!\n";*)
      (* 3.代码生成*)
      let asm = Codegenerator.codegen_program ast in
      (*Printf.printf "Generated RISC-V Assembly:\n\n";*)
      List.iter (Printf.printf "%s\n") asm;
      true
    end else begin
      Printf.eprintf "Semantic check failed with errors:\n";
      false
    end
  with
  | Lexer.Lexical_error ->
      print_error_position lexbuf;
      Printf.eprintf "Lexical error (invalid token)\n";
      false
  | Parser.Syntax_error ->
      print_error_position lexbuf;
      Printf.eprintf "Syntax error\n";
      false
  | ex ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string ex);
      false

(* 测试用例 *)
let test_cases = [
  (* 合法程序 *)
  ( "valid.toyc",
    "int main() { int x = 1; return x; }",
    true
  );
  (* 缺少main函数 *)
  ( "no_main.toyc",
    "void foo() {}",
    false
  );
  (* 未定义变量 *)
  ( "undefined_var.toyc",
    "int main() { y = 1; }",
    false
  )
]

(* 运行所有测试 *)
let run_tests () =
  List.iter (fun (name, code, expected) ->
    Printf.printf "\n=== Testing %s ===\n" name;
    let result = process_source code in
    if result = expected then
      Printf.printf "[PASS] Test '%s' succeeded\n" name
    else
      Printf.printf "[FAIL] Test '%s' (expected %b, got %b)\n" 
        name expected result
  ) test_cases

(* 主入口 *)
let () =
  (* 从标准输入读取或运行测试 *)
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--test" then
    run_tests ()
  else begin
    (*Printf.printf "Enter ToyC code (end with Ctrl+D):\n";*)
    let code = In_channel.input_all stdin in
    ignore (process_source code)
end