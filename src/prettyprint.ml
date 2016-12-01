(* Pretty Printer BRUG*)

open Ast
open Parser
open Scanner


(* Print Data Types BRUG*)

let rec string_of_types = function
  Int_t             -> "int"
  | Bool_t          -> "bool"
  | Double_t        -> "double"
  | Char_t          -> "char"
  | Unit_t          -> "unit"
  | String_t        -> "string"
  | List_t(t)       -> "list<" ^ (string_of_types t) ^ ">"
  | Set_t(t)        -> "set<" ^ (string_of_types t) ^ ">"
  | Map_t(t1, t2)   -> "map<" ^ (string_of_types t1) ^
                            ", " ^ (string_of_types t2) ^ ">"
  | Tuple_t(tlist)  -> "tup<" ^ (String.concat ", "
                            (List.map string_of_types tlist)) ^ ">"

let string_of_actor_type = function
  Actor_t(name) -> "actor<" ^ name ^ ">"

let string_of_pool_type = function
  Pool_t(Actor_t(name), cnt) -> "pool<" ^ name ^ ">" ^
      "(" ^ string_of_int cnt ^ ")"


(* Print Formals BRUG*)

let string_of_formal = function
  (name, typ) -> name ^ ": " ^ string_of_types typ

let string_of_formals formals =
  String.concat ", " (List.map string_of_formal formals)


(* Print Operators BRUG*)

let string_of_binop = function
  Add           -> "+"
  | Sub         -> "-"
  | Mult        -> "*"
  | Div         -> "/"
  | Mod         -> "%"
  | Equal       -> "=="
  | Neq         -> "!="
  | Less        -> "<"
  | Leq         -> "<="
  | Greater     -> ">"
  | Geq         -> ">="
  | And         -> "&&"
  | Or          -> "||"
  | Bit_RShift  -> ">>"
  | Bit_LShift  -> "<<"
  | Bit_And     -> "&"
  | Bit_Or      -> "|"
  | Bit_Xor     -> "^"

let string_of_uop = function
  Not           -> "!"
  | Neg         -> "-"

let string_of_actor_op = function
  Actor_send        -> "|>"
  | Actor_broadcast -> "|>>"


(* Print Messages BRUG*)

let string_of_message message =
  message.m_name ^ "(" ^ (string_of_formals message.m_formals) ^ ")"


(* Print Expressions BRUG*)

let rec string_of_expr = function
    Binop(e1, o, e2)	        -> "(" ^
                                  (string_of_expr e1) ^ " " ^
                                  (string_of_binop o) ^ " " ^
							                    (string_of_expr e2)
                                  ^ ")"
  | Unop(o, e)                -> (string_of_expr e) ^ (string_of_uop o)
  | Id(s)                     -> s
  | Assign(v, e)              -> v ^ " = " ^ string_of_expr e
  | Access(v, e)              -> v ^ "[" ^ string_of_expr e ^ "]"
  | Int_Lit(i)			          -> string_of_int i
  | Double_Lit(f)             -> string_of_float f
  | Char_Lit(c)               -> Char.escaped c
  | String_Lit(s)		          -> s
  | Bool_Lit(true)            -> "true"
  | Bool_Lit(false)           -> "false"
  | Unit_Lit(u)               -> "unit"
  | List_Lit(t, exprs)        -> "list<" ^ (string_of_types t) ^ ">[" ^
                                  (string_of_exprs exprs) ^ "]"
  | Set_Lit(t, exprs)         -> "set<" ^ (string_of_types t) ^ ">[" ^
                                  (string_of_exprs exprs) ^ "]"
  | Map_Lit(t1, t2, m_exprs)  -> "map<" ^ (string_of_types t1) ^
                                  ", " ^ (string_of_types t2) ^ ">[" ^
                                  (string_of_kvs m_exprs) ^ "]"
  | Tuple_Lit(ts, exprs)      -> "tup<" ^ (String.concat ", "
                                  (List.map string_of_types ts)) ^ ">(" ^
                                  (string_of_exprs exprs) ^ ")"
  | Lambda(lambda)            -> (string_of_lambda lambda)
  | Call(s, exprs)            -> s ^ "(" ^ string_of_exprs exprs ^ ")"
  | Actor_comm(m,o,a)         -> "(" ^ (string_of_message m) ^ " " ^
                                  (string_of_actor_op o) ^ " " ^
							                    (string_of_actor_type a) ^ ")"
  | Noexpr                    -> ""

and string_of_exprs exprs =
  String.concat ", " (List.map string_of_expr exprs)

(* helper to print map key-value pairs *)
and string_of_kvs kvs =
  let kv_string kv =
    let (k, v) = kv in string_of_expr k ^ " -> " ^ string_of_expr v in
  String.concat ", " (List.map kv_string kvs)

(* Print Statements BRUG*)

and string_of_stmt = function
    Block(stmts)                  -> "{\n" ^ string_of_stmts stmts ^ "\n}\n"
  | Expr(expr)                    -> string_of_expr expr ^ ";\n"
  | Return(expr)                  -> "return " ^ string_of_expr expr ^ ";\n"
  | Mut(v, t)                     -> "mut " ^ string_of_types t ^ " " ^
                                      v ^ ";\n"
  | Mutdecl(v, t, e)              -> "mut " ^  string_of_types t ^ " " ^ v ^
                                      string_of_expr e ^";\n"
  | Vdecl(v, t, e)                -> string_of_types t ^ " " ^ v ^
                                      string_of_expr e ^ ";\n"
  | If(e, s1, s2)                 -> "if (" ^ string_of_expr e ^ ") {\n" ^
                                      string_of_stmts s1 ^ "\n} else {\n" ^
                                      string_of_stmts s2 ^ "\n}\n"
  | For(v, f1, f2, f3, stmts)     -> "for (int " ^ v ^ " <- " ^
                                      string_of_int f1 ^ " to " ^
                                      string_of_int f2 ^ " by " ^
                                      string_of_int f3 ^ ") {\n" ^
                                      string_of_stmts stmts ^ "\n}\n"
  | While(e, stmts)               -> "while (" ^ (string_of_expr e) ^ ") {\n" ^
                                      string_of_stmts stmts ^ "\n}\n"
  | Spawn_act(t1, v, t2, exprs)   -> t1 ^ " " ^ v ^ " = spawn " ^ t2 ^ "(" ^
                                      string_of_exprs exprs ^ ");\n"
  | Spawn_pool(t1, v, t2, exprs)  -> "pool<" ^ t1 ^ "> " ^ v ^
                                      " = spawn pool<" ^ t2 ^ ">(" ^
                                      string_of_exprs exprs ^ ");\n"
  | Break                         -> "break"
  | Continue                      -> "continue"

and string_of_stmts stmts =
  String.concat "\n" (List.map string_of_stmt stmts)


(* Print Lambda Functions BRUG*)

and string_of_lambda lambda =
  "(" ^ string_of_formals lambda.l_formals ^ ") => " ^ string_of_types
    lambda.l_return_t ^ " = {\n" ^ string_of_stmts lambda.l_body ^ "\n}"


(* Print Patterns BRUG*)

let string_of_pattern p =
  "| " ^ p.p_message_id ^ "(" ^ string_of_formals p.p_message_formals
    ^ ") => {\n        " ^ String.concat "\n        "
    (List.map string_of_stmt p.p_stmts) ^ "\n      }"


(* Print Functions BRUG*)

let string_of_func func =
  "def " ^ func.f_name ^ "(" ^ string_of_formals func.f_formals ^ ") => " ^
    string_of_types func.f_return_t ^ " = {\n" ^
    string_of_stmts func.f_body ^ "\n}"


(* Print Actor BRUG*)

let string_of_actor actor =
  "actor " ^ actor.a_name ^ "(" ^ string_of_formals actor.a_formals ^
  ") {\n  " ^ string_of_stmts actor.a_body ^ "\n\n  " ^
  String.concat "\n\n  " (List.map string_of_func actor.a_functions) ^
  "\n\n  receive {\n    " ^ String.concat "\n    "
  (List.map string_of_pattern actor.a_receive) ^ "\n  }\n}"


let string_of_program (messages, actors, funcs) =
  String.concat "\n" (List.map string_of_message messages) ^ "\n\n\n" ^
    String.concat "\n\n" (List.map string_of_actor actors) ^ "\n\n\n" ^
	  String.concat "\n\n" (List.map string_of_func funcs)

(* Words That Do Stuff *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = string_of_program program in
  print_endline result
