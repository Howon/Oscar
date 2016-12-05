(* Pretty Printer BRUG*)

open Ast
open Parser
open Scanner

(* Print Operators BRUG*)

let str_binop = function
    Add         -> "+"
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
  | Assign      -> "="
  | Access      -> ""

let str_uop = function
    Not         -> "!"
  | Neg         -> "-"

(* Print Data Types BRUG*)

let rec str_types = function
    Int_t              -> "int"
  | Bool_t             -> "bool"
  | Double_t           -> "double"
  | Char_t             -> "char"
  | Unit_t             -> "unit"
  | String_t           -> "string"
  | Lambda_t(args, rt) -> "(" ^ (String.concat ", "
                            (List.map (fun arg ->
                                str_types arg) args)
                            ) ^ ") => " ^ str_types rt ^ ")"
  | List_t(t)          -> "list<" ^ (str_types t) ^ ">"
  | Set_t(t)           -> "set<" ^ (str_types t) ^ ">"
  | Map_t(t1, t2)      -> "map<" ^ (str_types t1) ^ ", " ^
                                (str_types t2) ^ ">"
  | Actor_t(t)         -> "actor<" ^ str_expr t ^ ">"
  | Pool_t(t)          -> "pool<" ^ str_expr t ^ ">"
  | Message_t(t)       -> "message<" ^ str_expr t ^ ">"

(* Print Formals BRUG*)

and str_formal = function
  (name, typ) -> name ^ ": " ^ str_types typ

and str_formals formals =
  String.concat ", " (List.map str_formal formals)

(* Print Messages BRUG*)

and str_message message =
  "message " ^ message.m_name ^ "(" ^ (str_formals message.m_formals) ^ ")"

(* Print Expressions BRUG*)

and str_expr = function
    Binop(e1, o, e2)          -> "(" ^ (str_expr e1) ^ (match o with
                                    Access -> "[" ^ (str_expr e2) ^ "]"
                                    | _ -> " " ^ (str_binop o) ^ " " ^
                                    (str_expr e2)) ^ ")"
  | Uop(o, e)                 -> (str_uop o) ^ (str_expr e)
  | Id(s)                     -> s
  | Int_Lit(i)                -> string_of_int i
  | Double_Lit(f)             -> string_of_float f
  | Char_Lit(c)               -> "\'" ^ Char.escaped c ^ "\'"
  | String_Lit(s)             -> "\"" ^ s ^ "\""
  | Bool_Lit(true)            -> "true"
  | Bool_Lit(false)           -> "false"
  | Unit_Lit(u)               -> "unit"
  | List_Lit(t, exprs)        -> "list<" ^ (str_types t) ^ ">[" ^
                                    (str_exprs exprs) ^ "]"
  | Set_Lit(t, exprs)         -> "set<" ^ (str_types t) ^ ">[" ^
                                    (str_exprs exprs) ^ "]"
  | Map_Lit(kt, vt, m_exprs)  -> "map<" ^ (str_types kt) ^ ", " ^
                                    (str_types vt) ^ ">[" ^
                                    (str_kvs m_exprs) ^ "]"
  | Actor_Lit(at, exprs)      -> "spawn actor<" ^ str_expr at ^ ">(" ^
                                    (str_exprs exprs) ^ ")"
  | Pool_Lit(at, exprs, num)  -> "spawn pool<" ^ str_expr at ^ ">({" ^
                                    (str_exprs exprs) ^ "}, " ^
                                    (str_expr num) ^ ")"
  | Message_Lit(m, exprs)     -> "message<" ^ str_expr m ^ ">(" ^
                                    str_exprs exprs ^ ")"
  | Lambda(lambda)            -> (str_lambda lambda)
  | Call(s, exprs)            -> str_expr s ^ "(" ^ str_exprs exprs ^ ")"
  | Noexpr                    -> ""

and str_exprs exprs =
  String.concat ", " (List.map str_expr exprs)

(* helper to print map key-value pairs *)
and str_kvs kvs =
  let kv_string kv =
    let (k, v) = kv in str_expr k ^ " -> " ^ str_expr v in
  String.concat ", " (List.map kv_string kvs)

(* Print Statements BRUG*)

and str_stmt = function
    Block(stmts)     -> "{\n" ^ str_stmts (Block(stmts)) ^ "\n}"
  | Expr(expr)       -> str_expr expr ^ ";"
  | Return(expr)     -> "return " ^ str_expr expr ^ ";"
  | Vdecl(v)         -> (str_types v.v_type) ^ " " ^ v.v_name ^
                           " = " ^ (str_expr v.v_init) ^ ";"
  | Mutdecl(mv)      -> "mut " ^ (str_types mv.mv_type) ^
                           " " ^ mv.mv_name ^ (match mv.mv_init with
                             Noexpr -> ""
                           | _ -> " = " ^ (str_expr mv.mv_init)) ^ ";"
  | If(e, s1, s2)    -> str_if e s1 s2
  | Actor_send(e, a) -> (str_expr e) ^ " |> " ^ str_expr a ^ ";"
  | Pool_send(e, p)  -> (str_expr e) ^ " |>> " ^ str_expr p ^ ";"

and str_stmts = function
  Block(stmts) -> String.concat "\n" (List.map str_stmt stmts)
  | _ -> ""

(* Print Lambda Functions BRUG*)

and str_if e s1 s2 =
  "if (" ^ str_expr e ^ ") " ^ str_stmt s1 ^ (match s2 with
    Expr(Noexpr) -> ""
    | _    -> " else " ^ str_stmt s2)

and str_lambda lambda =
  "(" ^ str_formals lambda.l_formals ^ ") => " ^ str_types
    lambda.l_return_t ^ " = " ^ str_stmt lambda.l_body

(* Print Patterns BRUG*)

let str_pattern p =
  "| " ^ p.p_mid ^ "(" ^ str_formals p.p_mformals
    ^ ") => " ^ str_stmt p.p_body

(* Print Functions BRUG*)

let str_func func =
  "def " ^ func.f_name ^ "(" ^ str_formals func.f_formals ^ ") => " ^
    str_types func.f_return_t ^ " = " ^ str_stmt func.f_body

(* Print Actor BRUG*)

let str_actor actor =
  "actor " ^ actor.a_name ^ "(" ^ str_formals actor.a_formals ^
  ") {\n" ^ str_stmts actor.a_body ^ "\n\n" ^
  "\n\nreceive = {\n" ^ String.concat "\n"
  (List.map str_pattern actor.a_receive) ^ "\n}\n}"

let str_program (messages, actors, funcs) =
  String.concat "\n" (List.map str_message messages) ^ "\n\n" ^
    String.concat "\n\n" (List.map str_actor actors) ^ "\n\n" ^
      String.concat "\n\n" (List.map str_func funcs)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = str_program program in
	print_endline result
