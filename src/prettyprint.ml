(* Pretty Printer BRUG*)
open Ast
open Parser
open Scanner
  
(* Print Data Types BRUG*)
  
let string_of_p_type = function
    Int_t      -> "int"
  | Bool_t     -> "bool"
  | Double_t   -> "double"
  | Char_t     -> "char"
  | Unit_t     -> "unit"
  
let string_of_cont_type = function
    String_t   -> "string"
  | List_t     -> "list"
  | Set_t      -> "set"
  | Map_t      -> "map"
  | Tuple_t    -> "tup"

let string_of_opt_type = function 
    Maybe_t -> "Maybe"
  | Some_t  -> "Some"
  
let string_of_types = function
    Primitive(p)    -> (string_of_p_type p)
  | Container(c)    -> (string_of_cont_type c)
  
let string_of_actor_type = function 
    Actor(s) -> s

(* Print Operators BRUG*)
	
let string_of_bin_op = function
    Add        -> "+"
  | Minus      -> "-"
  | Multiply   -> "*"
  | Divide     -> "/" 
  | Modulo     -> "%"
  | Equal      -> "=="
  | Neq        -> "!="
  | Lt         -> "<"
  | Leq        -> "<="
  | Gt         -> ">"
  | Geq        -> ">="
  | And        -> "&&"
  | Or         -> "||"
  
let string_of_u_op = function 
    Not        -> "!"
  | Neg        -> "-" 
  
let string_of_actor_op = function 
    Actor_send      -> "|>"
  | Actor_broadcast -> "|>>"
  | Actor_receive   -> "receive"

let string_of_bit_op = function
    Bit_RShift    -> ">>"
  | Bit_LShift    -> "<<"
  | Bit_And       -> "&"
  | Bit_Or        -> "|"
  | Bit_Xor       -> "^"
  | Bit_Not       -> "~"
  
(* Print Expressions BRUG*)
  
let rec string_of_expr = function
    Binop(e1, bn_o, e2)	-> "(" ^ (string_of_expr e1) ^ " " ^
	                         (string_of_bin_op b) ^ " " ^ (string_of_expr e2)
							   ^ ")"
  | Bitop(e1, bt_o, e2)	-> "(" ^ (string_of_int i) ^ " " ^ (string_of_bit_op b)
                             ^ " " ^ (string_of_int i) ^ ")"
  | Unop(u, e)          -> (string_of_expr e) ^ (string_of_u_op u)
  | Id(s)               -> s
  | Assign(v, e)        -> v ^ " = " ^ string_of_expr e
  | Int_Lit(i)			-> string_of_int i
  | Double_Lit(f)       -> string_of_float f
  | Char_Lit(c)         -> c
  | String_Lit(s)		-> s
  | Bool_Lit(true)      -> "true"
  | Bool_Lit(false)     -> "false"
  | List_init           -> ????????
  | Call(s, el)         -> s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Actor_comm(m,a_o,a)          -> "(" ^ (string_of_msg e1) ^ " " ^ (string_of_actor_op b) ^ " " ^ (string_of_actor a) ^ ")"
  | Noexpr              -> ""
  | _                   -> "brug"
  
(* Print Statements BRUG*)

let rec string_of_stmt = function 
    Block(stmts)        -> "{\n" ^ String.concat ""
	                         (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)          -> string_of_expr expr ^ ";\n";
  | Return(expr)        -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2)       -> "if (" ^ string_of_expr e ^ ")\n" ^
                             string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(fi, e2, e3, st) -> ("for (" ^ (string_of_for_init fi) ^ " ; " ^
                             (string_of_expr e2) ^ " ; " ^ (string_of_expr e3)
							   ^ ") " ^ (string_of_stmt st))
  | While(e, st)        -> ("while (" ^ (string_of_expr e) ^ ") " ^
                             (string_of_stmt st))
  | Break               -> "break"
  | Continue            -> "continue"

(* Print Patterns BRUG*) 
  
let string_of_pattern pattern =  
  pattern.message_id ^ " " ^ "(" ^ String.concat ", " 
    (List.map string_of_formal pattern.message_formals) ^ 
	  ")\n{\n" ^ String.concat "" (List.map string_of_stmt pattern.stmts) ^ "}\n"


(* Words That Do Stuff *) 
  
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = string_of_expr expr in
  print_endline result
  
  
	