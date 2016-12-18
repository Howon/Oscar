(* Top-level of the Oscar compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)
open Core.Std

open Sast
open Lexing
open Printf
open Transpile
open Sys

type action = Compile | Ast | Sast | Llvm_gen

let make_lexbuf file =
  let lexbuf = Lexing.from_channel file in
  let curr_p = {lexbuf.lex_curr_p with pos_lnum=1} in
  {lexbuf with lex_curr_p = curr_p;}


let scan_error lexbuf error = 
  let s = lexeme_start_p lexbuf in
  let f = lexeme_end_p lexbuf in
    (fprintf stderr "Scanner: Unrecgonized char at line %d, char %d - %d: \"%s\" \n"
    s.pos_lnum (s.pos_cnum - s.pos_bol)
    (f.pos_cnum - f.pos_bol) error)

let parse_error lexbuf =
  let s = lexeme_start_p lexbuf in
  let f = lexeme_end_p lexbuf in
    (fprintf stderr "Parser: Syntax error at line %d, char %d - %d: \"%s\" \n"
    s.pos_lnum (s.pos_cnum - s.pos_bol)
    (f.pos_cnum - f.pos_bol) (Lexing.lexeme lexbuf))

let _ =
  let arg_len = Array.length Sys.argv in
  let (action, oscar) =
    (if (arg_len = 1 || arg_len = 4 || arg_len > 5) then
      let _ = print_endline("Usage: ./oscar [-p|-s|-c|-l|] *.oscar [-o outfile]") in
      print_endline (string_of_int arg_len);
      exit 1;
    else
      try
        (List.Assoc.find_exn [
                ("-p", Ast);          (* Prettyprint Ast *)
                ("-s", Sast);         (* Prettyprint Sast *)
                ("-c", Compile);      (* Generate cpp and executable *)
                ("-l", Llvm_gen);     (* Generate cpp and llvm *)
        ] Sys.argv.(1), Sys.argv.(2))
      with Not_found ->
        raise (Failure ("Invalid flag " ^ Sys.argv.(1)))) 
  in
  let lexbuf = make_lexbuf (open_in oscar)
  and stdlex = make_lexbuf (open_in "include/stdlib.oscar") in
  let program =
    try
      Parser.program Scanner.token lexbuf
    with
        Scanner.Scan_error(f) -> let () = ignore(scan_error lexbuf f) in exit(1)
      | Parsing.Parse_error   -> let () = ignore(parse_error lexbuf) in exit(1)
  and stdlib = Parser.program Scanner.token stdlex in
  match action with
      Ast  -> print_endline (Ast.str_program program)
    | _    ->
         let sprogram =
           try
             Analyzer.check_program program stdlib
           with
             Failure f ->
               Printf.eprintf "%s" ("Error: " ^ f);
               flush stderr;
               exit 1;
         in
         match action with
            Ast   -> ()
          | Sast  -> print_endline (Sast.str_sprogram sprogram)
          | _ ->
              let program = Transpile.c_program sprogram in
              let file_stub =
                  let rdot = String.rindex oscar '.' in
                  let rdot = (match rdot with
                        Some(i) -> i
                      | None -> raise (Failure ("Filename should be .oscar")))
                  in
                  String.sub oscar 0 rdot
              in
              let cpp_file = file_stub ^ ".cpp" in
              match action with
                  Ast      -> ()
                | Sast     -> ()
                | Llvm_gen -> 
                    let c_op = "-Wall -pedantic -fsanitize=address -std=c++1y -O2 -S -emit-llvm" in
                    let cxx_incls = "-I/usr/local/include/ " in
                    let cxx = sprintf "clang++ %s %s " c_op cxx_incls ^ cpp_file ^ " -o " ^ file_stub ^ ".ll" in
                    Out_channel.write_all cpp_file ~data:program;
                    let ch = Unix.open_process_out cxx in
                      Out_channel.output_string ch program;
                | Compile ->
                    let exec_file = (if arg_len = 5 then Sys.argv.(4) else file_stub) in
                    let c_op = "-Wall -pedantic -fsanitize=address -std=c++1y -O2" in
                    let cxx_incls = "-I/usr/local/include/ -L/usr/local/lib/ " in
                    let cxx = sprintf "clang++ %s %s " c_op cxx_incls ^ cpp_file ^ " -o " ^ exec_file in
                    Out_channel.write_all cpp_file ~data:program;
                    let ch = Unix.open_process_out cxx in
                      Out_channel.output_string ch program;
