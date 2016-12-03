type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift | Assign | Access

type u_op = Not | Neg

type types = Int_t | Bool_t | Double_t | Char_t | Unit_t | String_t
  | Lambda_t of (types list) * types
  | List_t of types
  | Set_t of types
  | Map_t of types * types
  | Actor_t of string
  | Pool_t of string

type formal = string * types

type message = {
    m_name: string;
    m_formals: formal list;
}

type expr =
    Int_Lit of int
  | Double_Lit of float
  | Char_Lit of char
  | String_Lit of string
  | Bool_Lit of bool
  | Unit_Lit of unit
  | List_Lit of types * expr list
  | Set_Lit of types * expr list
  | Map_Lit of types * types * (expr * expr) list
  | Actor_Lit of string * (expr list)
  | Pool_Lit of string * (expr list) * expr
  | Binop of expr * bin_op * expr
  | Uop of u_op * expr
  | Id of string
  | Lambda of lambda
  | Call of string * expr list
  | Noexpr

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

and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Vdecl of val_decl
  | Mutdecl of mvar_decl
  | If of expr * stmt list * stmt list
  | For of string * int * int * int * stmt list
  | While of expr * stmt list
  | Break
  | Continue
  | Actor_send of message * (expr list) * expr
  | Pool_send of message * (expr list) * expr

and lambda = {
  l_formals: formal list;
  l_return_t: types;
  l_body: stmt list;
}

and pattern = {
  p_message_id: string;
  p_message_formals: formal list;
  p_stmts: stmt list;
}

and func = {
  f_name: string;
  f_formals: formal list;
  f_return_t: types;
  f_body: stmt list;
}

and actor = {
  a_name: string;
  a_formals: formal list;
  a_body: stmt list;
  a_functions: func list;
  a_receive: pattern list;
}

type program = message list * actor list * func list
