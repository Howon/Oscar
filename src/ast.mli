type program = message list * actor list * func list

type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift

type u_op = Not | Neg | Bit_Not

type p_type = Int_t | Bool_t | Double_t | Char_t | Unit_t

type cont_type = String_t | List_t | Set_t | Map_t | Tuple_t

type actor_op =
    Actor_send
  | Actor_broadcast
  | Actor_receive

type types = Primitive of p_type | Container of cont_type

type opt_type = Maybe_t | Some_t 

type optional_rec = {
    type_: opt_type;
    param: types;
}

type optional = Optional_t of optional_rec | None_t

type formal = string * types

type field = Field of types * string

type actor_type = Actor_t of string

type pool_type = Pool_t of actor_type list

and expr =
    Binop of expr * bin_op * expr
  | Unop of u_op * expr
  | Id of string
  | Assign of string * expr
  | Int_Lit of int
  | Double_Lit of float
  | Char_Lit of char
  | String_Lit of string
  | Bool_Lit of bool
  | List_init of expr list
  | Call of string * expr list
  | Actor_comm of message_type * actor_op * actor_type
  | Noexpr

and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue


type message = {
    name: string;
    formals: formal list;
}

type pattern = {
  message_id: string;
  message_formals: formals list;
  stmts: stmt list;
}

type actor = {
  name: string;
  formals: formal list;
  body: stmt list;
  functions: func list;
  receive: pattern list;
}

type func = {
  name: string;
  formals: formal list;
  return_t: types;
  body: stmt list;
}

type lambda = {
  formals: formal list;
  return_t: types;
  body: stmt list;
}
