(* Top-level of the Oscar compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

open Ast

type action = LLVM_IR | Compile | PrettyPrint

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [
            ("-l", LLVM_IR);    (* Generate LLVM, don't check *)
            ("-c", Compile);    (* Generate, check LLVM IR *)
            ("-p", PrettyPrint) (* Don't generate LLVM, just prettyprint *)
    ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  (* let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast; *)
  match action with

  PrettyPrint -> print_endline (Ast.str_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
