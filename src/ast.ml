type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift | Assign | Access

type u_op = Not | Neg

type types = Int_t | Bool_t | Double_t | Char_t | Unit_t | String_t
  | Lambda_t  of (types list) * types
  | List_t    of types
  | Set_t     of types
  | Map_t     of types * types
  | Actor_t   of expr
  | Pool_t    of expr
  | Message_t of expr

and expr =
    Int_Lit     of int
  | Double_Lit  of float
  | Char_Lit    of char
  | String_Lit  of string
  | Bool_Lit    of bool
  | Unit_Lit    of unit
  | List_Lit    of types * expr list
  | Set_Lit     of types * expr list
  | Map_Lit     of types * types * (expr * expr) list
  | Actor_Lit   of expr * (expr list)
  | Pool_Lit    of expr * (expr list) * expr
  | Message_Lit of expr * (expr list)
  | Binop       of expr * bin_op * expr
  | Uop         of u_op * expr
  | Id          of string
  | Lambda      of lambda
  | Call        of expr * expr list
  | Noexpr

and stmt =
    Block      of stmt list
  | Expr       of expr
  | Return     of expr
  | Vdecl      of val_decl
  | Mutdecl    of mvar_decl
  | If         of expr * stmt * stmt
  | Actor_send of expr * expr
  | Pool_send  of expr * expr

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
  a_functions : func list;
  a_receive   : pattern list;
}

type program = message list * actor list * func list
