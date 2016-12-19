open Core.Std

open Sast
open Lexing
open Printf
open Transpile
open Optimizer
open Sys

type action = Compile | Ast | Sast | Llvm_gen

let make_lexbuf file =
  let lexbuf = Lexing.from_channel file in
  let curr_p = {lexbuf.lex_curr_p with pos_lnum=1} in
  {lexbuf with lex_curr_p = curr_p;}

let scan_error lexbuf error = 
  let s = lexeme_start_p lexbuf in
  let f = lexeme_end_p lexbuf in
    (fprintf stderr 
            "Scanner: Unrecgonized char at line %d, char %d - %d: \"%s\" \n"
    s.pos_lnum (s.pos_cnum - s.pos_bol)
    (f.pos_cnum - f.pos_bol) error)

let parse_error lexbuf =
  let s = lexeme_start_p lexbuf in
  let f = lexeme_end_p lexbuf in
    (fprintf stderr "Parser: Syntax error at line %d, char %d - %d: \"%s\" \n"
    s.pos_lnum (s.pos_cnum - s.pos_bol)
    (f.pos_cnum - f.pos_bol) (Lexing.lexeme lexbuf))

let get_file_stub filename = 
  let rdot = String.rindex filename '.' in
  let rdot = (match rdot with
        Some(i) -> i
      | None -> raise (Failure ("Filename should be .oscar")))
  in
  String.sub filename 0 rdot

let _ =
  let arg_len = Array.length Sys.argv in
    let (action, optimize, oscar) =
    (if (arg_len < 2 || arg_len > 4) then
      let _ = 
          print_endline("Usage: ./oscar [-p|-s|-l|-c] [?-O] *.oscar") in
      exit 1;
    else if arg_len = 2 then
      (Compile, true, Sys.argv.(1))
    else if Sys.argv.(2) = "-O" then
      try
        (List.Assoc.find_exn [
                ("-p", Ast);      (* Prettyprint ast *)
                ("-s", Sast);     (* Prettyprint sast *)
                ("-l", Llvm_gen); (* Generate cpp and llvm *)
                ("-c", Compile);  (* Generate cpp and executable *)
        ] Sys.argv.(1), true, Sys.argv.(3))
      with Not_found ->
        raise (Failure ("Invalid flag " ^ Sys.argv.(1)))
    else if arg_len = 3 then
      try
        (List.Assoc.find_exn [
                ("-p", Ast);      (* Prettyprint ast *)
                ("-s", Sast);     (* Prettyprint sast *)
                ("-l", Llvm_gen); (* Generate cpp and llvm *)
                ("-c", Compile);  (* Generate cpp and executable *)
        ] Sys.argv.(1), false, Sys.argv.(2))
      with Not_found ->
        raise (Failure ("Invalid flag " ^ Sys.argv.(1)))
    else raise (Failure ("Invalid flag " ^ Sys.argv.(2)))) in
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
        let soprogram =
          let sprogram =
            try
              Analyzer.check_program program stdlib
            with
              Failure f ->
                Printf.eprintf "%s" ("Error: " ^ f);
                flush stderr;
                exit 1;
          in
          if optimize then
            try
              Optimizer.optimize_program sprogram
            with
              Failure f ->
                Printf.eprintf "%s" ("Error: " ^ f);
                flush stderr;
                exit 2;
          else
            sprogram
        in
        match action with
            Ast   -> ()
          | Sast  -> print_endline (Sast.str_sprogram soprogram)
          | _ ->
              let program = Transpile.c_program soprogram in
              let file_stub = get_file_stub oscar in
              let cpp_file = file_stub ^ ".cpp" in
              let c_op = "-Wall -pedantic -fsanitize=address -std=c++1y -O2" in
              let cxx_incls = "-I/usr/local/include/ " in
              let cxx = (match action with
                    Ast      -> ""
                  | Sast     -> ""
                  | Llvm_gen -> 
                      let c_op = c_op ^ " -S -emit-llvm" in
                      sprintf "clang++ %s %s " c_op cxx_incls ^ cpp_file ^ 
                                                      " -o " ^ file_stub ^ ".ll"
                  | Compile ->
                      let cxx_incls = cxx_incls ^ "-L/usr/local/lib/ " in
                      sprintf "clang++ %s %s " c_op cxx_incls ^ cpp_file ^ 
                                                       " -o " ^ file_stub)
              in
              let ch = Unix.open_process_out cxx in
              Out_channel.write_all cpp_file ~data:program;
              Out_channel.output_string ch program;