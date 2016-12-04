(* Top-level of the Oscar compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
                  ("-s", Sast);
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram =
    try
      Analyzer.check_program program
    with
      Failure f ->
        (print_endline("Error: " ^ f);
        fflush stdout;
        exit 1;)
  in
  (* let ast = Parser.program Scanner.token lexbuf in
    Semant.check ast; *)
    match action with
     (* Ast -> print_string (Ast.string_of_program ast) *)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate program))
    | Compile -> let m = Codegen.translate ast in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)

