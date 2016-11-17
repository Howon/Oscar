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

(*
type types = Primitive of p_type | Container of cont_type
*)

type types = Int_t | Bool_t | Double_t | Char_t | Unit_t |  String_t
  | List_t of types
  | Set_t of types
  | Map_t of types * types
  | Tuple_t of types list

(*
type p_type = Int_t | Bool_t | Float_t | Char_t | Unit_t

type cont_type =
    String_t
  | List_t of types
  | Set_t of types
  | Map_t of types * types
  | Tuple_t of types list
*)



(* Commenting out optional types for now *)
(*type opt_type = Maybe_t | Some_t

type optional_rec = {
    type_: opt_type;
    param: types;
}

type optional = Optional_t of optional_rec | None_t*)

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
  | Int_Lit of int
  | Double_Lit of float
  | Char_Lit of char
  | String_Lit of string
  | Bool_Lit of bool
  | Unit_Lit of unit
(* | List_Lit of List_t * expr list
  | Set_Lit of Set_t * expr list
  | Map_Lit of Map_t * (expr * expr) list
  | Tuple_Lit of Tuple_t * expr list *)
  | Call of string * expr list
  | Actor_comm of message * actor_op * actor_type
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Mut of string * types
  | Vdecl
  | If of expr * stmt * stmt list
  | For of string * int * int * int * stmt list
  | While of expr * stmt list
  | Break
  | Continue

type pattern = {
  p_message_id: string;
  p_message_formals: formal list;
  p_stmts: stmt list;
}

type func = {
  f_name: string;
  f_formals: formal list;
  f_return_t: types;
  f_body: stmt list;
}

type actor = {
  a_name: string;
  a_formals: formal list;
  a_body: stmt list;
  a_functions: func list;
  a_receive: pattern list;
}

type lambda = {
  l_formals: formal list;
  l_return_t: types;
  l_body: stmt list;
}

type program = message list * actor list * func list
