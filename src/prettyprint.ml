(* Pretty Printer Executable *)
open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = str_program program in
	print_endline result
