(* Top-level of the Oscar compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

open Sast

type action = LLVM_IR | Compile | Sast

let _ =
  let arg_len = Array.length Sys.argv in
  let (action, oscar) =
    (if (arg_len = 1 || arg_len > 3) then
      let _ = print_endline("Usage: ./oscar [-l|-c|-s] *.oscar") in
      exit 1;
    else if arg_len > 1 then
        (List.assoc Sys.argv.(1) [
                ("-l", LLVM_IR);    (* Generate LLVM, don't check *)
                ("-c", Compile);    (* Generate, check LLVM IR *)
                ("-s", Sast) (* Don't generate LLVM, just prettyprint *)
        ], open_in Sys.argv.(2))
    else
      (Compile, open_in Sys.argv.(1))
    ) in
  let lexbuf = Lexing.from_channel oscar in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram =
    try
      Analyzer.check_program program
    with
      Failure f ->
        print_endline("Error: " ^ f);
        flush stdout;
        exit 1;
  in
(* let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast; *)
  match action with
   (* Ast -> print_string (Ast.string_of_program ast) *)
    Sast     -> print_endline (Sast.str_sprogram sprogram)
  | LLVM_IR  -> print_string
      (Llvm.string_of_llmodule (Codegen.translate program))
  | Compile  -> let m = Codegen.translate program in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)
