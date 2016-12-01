type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift

type u_op = Not | Neg

type actor_op =
    Actor_send
  | Actor_broadcast

type types = Int_t | Bool_t | Double_t | Char_t | Unit_t |  String_t
  | List_t of types
  | Set_t of types
  | Map_t of types * types
  | Tuple_t of types list

type formal = string * types

type message = {
    m_name: string;
    m_formals: formal list;
}

type field = Field of types * string

type actor_type = Actor_t of string

type pool_type = Pool_t of actor_type * int

type expr =
    Binop of expr * bin_op * expr
  | Unop of u_op * expr
  | Id of string
  | Assign of string * expr
  | Access of string * expr
  | Int_Lit of int
  | Double_Lit of float
  | Char_Lit of char
  | String_Lit of string
  | Bool_Lit of bool
  | Unit_Lit of unit
  | List_Lit of types * expr list
  | Set_Lit of types * expr list
  | Map_Lit of types * types * (expr * expr) list
  | Tuple_Lit of types list * expr list
  | Lambda of lambda
  | Call of string * expr list
  | Actor_comm of message * actor_op * actor_type
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
  | Spawn_act of string * string * string * expr list
  | Spawn_pool of string * string * string * expr list
  | Break
  | Continue

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
