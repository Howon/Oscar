type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift | Assign

type u_op = Not | Neg

type types = Int_t | Bool_t | Double_t | Char_t | Unit_t | String_t
  | Lambda_t   of types list * types
  | List_t     of types
  | Set_t      of types
  | Map_t      of types * types
  | Actor_t    of string
  | Pool_t     of string
  | Message_t  of string

and expr =
    Int_Lit      of int
  | Double_Lit   of float
  | Char_Lit     of char
  | String_Lit   of string
  | Bool_Lit     of bool
  | Unit_Lit     of unit
  | Id           of string
  | Access       of expr * expr
  | Lambda       of lambda
  | List_Lit     of types * expr list
  | Set_Lit      of types * expr list
  | Map_Lit      of types * types * (expr * expr) list
  | Actor_Lit    of string * (expr list)
  | Pool_Lit     of string * (expr list) * expr
  | Message_Lit  of string * (expr list)
  | Binop        of expr * bin_op * expr
  | Uop          of u_op * expr
  | FuncCall     of string * expr list
  | ObjCall      of expr * string * expr list
  | Noexpr

and stmt =
    Block       of stmt list
  | Expr        of expr
  | Return      of expr
  | Vdecl       of val_decl
  | Mutdecl     of mvar_decl
  | Fdecl       of func
  | If          of expr * stmt * stmt
  | Actor_send  of expr * expr
  | Pool_send   of expr * expr

and formal = string * types

and message = {
    m_name: string;
    m_formals: formal list;
}

and val_decl = {
    v_name : string;
    v_type : types;
    v_init : expr;
}

and mvar_decl = {
    mv_name : string;
    mv_type : types;
    mv_init : expr;
}

and lambda = {
  l_formals  : formal list;
  l_return_t : types;
  l_body     : stmt;
}

and pattern = {
  p_mid       : string;
  p_mformals  : formal list;
  p_body      : stmt;
}

and func = {
  f_name     : string;
  f_formals  : formal list;
  f_return_t : types;
  f_body     : stmt;
}

and actor = {
  a_name      : string;
  a_formals   : formal list;
  a_body      : stmt;
  a_receive   : pattern list;
}

type program = message list * actor list * func list

(* PRETTY PRINTER *)
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

let str_uop = function
    Not  -> "!"
  | Neg  -> "-"

(* Print Data Types BRUG*)

let rec str_types = function
    Int_t                -> "int"
  | Bool_t               -> "bool"
  | Double_t             -> "double"
  | Char_t               -> "char"
  | Unit_t               -> "unit"
  | String_t             -> "string"
  | Lambda_t (args, rt)  -> "(" ^ (String.concat ", " (List.map (fun arg ->
                              str_types arg) args)) ^ ") => " ^
                                str_types rt ^ ")"
  | List_t t             -> "list<" ^ str_types t ^ ">"
  | Set_t t              -> "set<" ^ str_types t ^ ">"
  | Map_t (t1, t2)       -> "map<" ^ str_types t1 ^ ", " ^ str_types t2 ^ ">"
  | Actor_t t            -> "actor<" ^ t ^ ">"
  | Pool_t t             -> "pool<" ^ t ^ ">"
  | Message_t t          -> "message<" ^ t ^ ">"

and str_types_list types =
  String.concat ", " (List.map str_types types)
(* Print Formals BRUG*)

and str_formal  = function
  (name, typ) -> name ^ " : " ^ str_types typ

and str_formals (formals : formal list) =
  String.concat ", " (List.map str_formal formals)

(* Print Messages BRUG*)

and str_message message =
  "message " ^ message.m_name ^ "(" ^ (str_formals message.m_formals) ^ ")"

(* Print Expressions BRUG*)

and str_expr = function
    Int_Lit i               -> string_of_int i
  | Double_Lit f            -> string_of_float f
  | Char_Lit c              -> "\'" ^ Char.escaped c ^ "\'"
  | String_Lit s            -> "\"" ^ s ^ "\""
  | Bool_Lit true           -> "true"
  | Bool_Lit false          -> "false"
  | Unit_Lit u              -> "unit"
  | Id s                    -> s
  | Access (cont, it)       -> str_expr cont ^ "[" ^ str_expr it ^ "]"
  | Lambda lambda           -> str_lambda lambda
  | List_Lit (t, ex)        -> "list<" ^  str_types t  ^ ">[" ^
                                str_exprs ex  ^ "]"
  | Set_Lit (t, ex)         -> "set<" ^  str_types t  ^ ">[" ^
                                str_exprs ex  ^ "]"
  | Map_Lit (kt, vt, kvs)   -> "map<" ^ str_types kt ^ ", " ^
                                str_types vt ^ ">[" ^ str_kvs kvs ^ "]"
  | Actor_Lit (at, ex)      -> "spawn actor<" ^ at ^ ">(" ^
                                str_exprs ex ^ ")"
  | Pool_Lit (at, ex, num)  -> "spawn pool<" ^ at ^ ">({" ^
                                str_exprs ex ^ "}, " ^
                                  str_expr num ^ ")"
  | Message_Lit (m, ex)    -> "message<" ^ m ^ ">(" ^
                                str_exprs ex ^ ")"
  | Binop (e1, o, e2)       -> "(" ^ str_expr e1 ^ " " ^ str_binop o ^
                                 " " ^ str_expr e2 ^ ")"
  | Uop (o, e)              -> str_uop o ^ str_expr e

  | FuncCall (s, ex)       -> s ^ "(" ^ str_exprs ex ^ ")"
  | ObjCall(obj, m, ex)    -> str_expr obj ^ "." ^ m ^ "(" ^ str_exprs ex ^ ")"
  | Noexpr                 -> ""

and str_exprs ex =
  String.concat ", " (List.map str_expr ex)

(* helper to print map key-value pairs *)
and str_kvs kvs =
  let kv_string kv =
    let (k, v) = kv in str_expr k ^ " -> " ^ str_expr v in
  String.concat ", " (List.map kv_string kvs)

(* Print Statements BRUG*)

and str_stmt = function
    Block stmts        -> "{\n" ^ str_stmts (Block(stmts)) ^ "\n}"
  | Expr expr          -> str_expr expr ^ ";"
  | Return expr        -> "return " ^ str_expr expr ^ ";"
  | Mutdecl mv         -> "mut " ^ str_types mv.mv_type ^ " " ^
                            mv.mv_name ^ (match mv.mv_init with
                               Noexpr -> ""
                             | _ -> " = " ^ str_expr mv.mv_init) ^ ";"
  | Vdecl v            -> str_types v.v_type ^ " " ^ v.v_name ^ " = " ^
                            str_expr v.v_init ^ ";"
  | Fdecl f            -> str_func f
  | If (e, s1, s2)     -> str_if e s1 s2
  | Actor_send (e, a)  -> str_expr e ^ " |> " ^ str_expr a ^ ";"
  | Pool_send (e, p)   -> str_expr e ^ " |>> " ^ str_expr p ^ ";"

and str_stmts = function
  Block(stmts)  -> String.concat "\n" (List.map str_stmt stmts)
  | _           -> ""

(* Print Lambda Functions BRUG*)

and str_if e s1 s2 =
  "if (" ^ str_expr e ^ ") " ^ str_stmt s1 ^ (match s2 with
      Expr Noexpr  -> ""
    | _            -> " else " ^ str_stmt s2)

and str_lambda lambda =
  "(" ^ str_formals lambda.l_formals ^ ") => " ^ str_types
    lambda.l_return_t ^ " = " ^ str_stmt lambda.l_body

(* Print Patterns BRUG*)

and str_pattern p =
  "| " ^ p.p_mid ^ "(" ^ str_formals p.p_mformals ^ ") => " ^ str_stmt p.p_body

(* Print Functions BRUG*)

and str_func func =
  "def " ^ func.f_name ^ "(" ^ str_formals func.f_formals ^ ") => " ^
    str_types func.f_return_t ^ " = " ^ str_stmt func.f_body

(* Print Actor BRUG*)

let str_actor actor =
  "actor " ^ actor.a_name ^ "(" ^ str_formals actor.a_formals ^ ") {\n" ^
    str_stmts actor.a_body ^ "\n\n" ^ "\n\nreceive = {\n" ^ String.concat "\n"
      (List.map str_pattern actor.a_receive) ^ "\n}\n}"

let str_program (messages, actors, funcs) =
  String.concat "\n" (List.map str_message messages) ^ "\n\n" ^
    String.concat "\n\n" (List.map str_actor actors) ^ "\n\n" ^
      String.concat "\n\n" (List.map str_func funcs)
