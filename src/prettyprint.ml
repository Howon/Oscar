(* Pretty Printer BRUG*)

open Ast
open Parser
open Scanner

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
  | Actor_t(t)         -> "actor<" ^ t ^ ">"
  | Pool_t(t)          -> "pool<" ^ t ^ ">"

(* Print Formals BRUG*)

let str_formal = function
  (name, typ) -> name ^ ": " ^ str_types typ

let str_formals formals =
  String.concat ", " (List.map str_formal formals)

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

(* Print Messages BRUG*)

let str_message message =
  message.m_name ^ "(" ^ (str_formals message.m_formals) ^ ")"


(* Print Expressions BRUG*)

let rec str_expr = function
    Binop(e1, o, e2)          -> "(" ^ (str_expr e1) ^ (match o with
                                     Access -> "[" ^ (str_expr e2) ^ "]"
                                   | _ -> " " ^ (str_binop o) ^ " " ^ (str_expr e2)) ^
                                   ")"
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
  | Actor_Lit(at, exprs)      -> "actor<" ^ str_expr at ^ ">" ^ str_exprs exprs
  | Pool_Lit(at, exprs, num)  -> "pool<" ^ str_expr at ^ ">" ^ (str_exprs exprs) ^
                                     (str_expr num)
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
    Block(stmts)              -> "{\n" ^ str_stmts stmts ^ "\n}"
  | Expr(expr)                -> str_expr expr ^ ";"
  | Return(expr)              -> "return " ^ str_expr expr ^ ";"
  | Mutdecl(mv)               -> "mut " ^ (str_types mv.mv_type) ^
                                    " " ^ mv.mv_name ^ (match mv.mv_init with
                                    Noexpr -> ""
                                    | _ -> " = " ^ (str_expr mv.mv_init)) ^ ";"
  | Vdecl(v)                  -> (str_types v.v_type) ^ " " ^ v.v_name ^
                                 " = " ^ (str_expr v.v_init) ^ ";"
  | If(e, s1, s2)             -> "if (" ^ str_expr e ^ ") {\n" ^
                                     str_stmt s1 ^ "\n} else {\n" ^
                                     str_stmt s2 ^ "\n}"
  | For(s, f1, f2, f3, stmts) -> "for (mut " ^ (str_stmt s) ^ " <- " ^
                                     (str_expr f1) ^ " to " ^
                                     (str_expr f2) ^ " by " ^
                                     (str_expr f3) ^ ") {\n" ^
                                     (str_stmt stmts) ^ "\n}"
  | While(e, stmts)           -> "while (" ^ (str_expr e) ^ ") {\n" ^
                                     str_stmt stmts ^ "\n}"
  | Break                     -> "break;"
  | Continue                  -> "continue;"
  | Actor_send(m, exprs, a)   -> (str_expr m) ^ "(" ^ str_exprs exprs ^
                                    ") |> " ^ str_expr a
  | Pool_send(m, exprs, p)    -> (str_expr m) ^ "(" ^ str_exprs exprs ^
                                    ") |>> " ^ str_expr p

and str_stmts stmts =
  String.concat "\n" (List.map str_stmt stmts)


(* Print Lambda Functions BRUG*)

and str_lambda lambda =
  "(" ^ str_formals lambda.l_formals ^ ") => " ^ str_types
    lambda.l_return_t ^ " = {\n" ^ str_stmt lambda.l_body ^ "\n}"

(* Print Patterns BRUG*)

let str_pattern p =
  "| " ^ p.p_message_id ^ "(" ^ str_formals p.p_message_formals
    ^ ") => {\n" ^ str_stmt p.p_body ^ "\n      }"

(* Print Functions BRUG*)

let str_func func =
  "def " ^ func.f_name ^ "(" ^ str_formals func.f_formals ^ ") => " ^
    str_types func.f_return_t ^ " = {\n" ^
    str_stmt func.f_body ^ "\n}"

(* Print Actor BRUG*)

let str_actor actor =
  "actor " ^ actor.a_name ^ "(" ^ str_formals actor.a_formals ^
  ") {\n" ^ str_stmt actor.a_body ^ "\n\n" ^
  String.concat "\n\n" (List.map str_func actor.a_functions) ^
  "\n\nreceive {\n" ^ String.concat "\n"
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
