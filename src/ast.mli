type program = None

type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or

type u_op = Not | Neg

type p_type = Int_t | Bool_t | Double_t | Char_t | Unit_t

type cont_type = String_t | List_t | Set_t | Map_t | Tuple_t

type actor_op =
    Actor_send
  | Actor_broadcast
  | Actor_receive

type types =
    Primitive of p_type
  | Container of cont_type
  | None_type

type opt_type = Maybe_t | Some_t | None_t

type optional = {
    type_: opt_type;
    param: types;
}

type formal = string * types

type field = Field of types * string

type actor_type = Actor_t of string

type pool_type = Pool_t of actor_type list

type bit_op =
    Big_RShift
  | Bit_LShift
  | Bit_And
  | Bit_Or
  | Bit_Xor
  | Bit_Not

type message_type = {
    name: string;
    body: stmt list;
}

and expr =
    Binop of expr * bin_op * expr
  | Bitop of int * bit_op * int
  | Uop of u_op * expr
  | Id of string
  | Assign of string * expr
  | Int_lit of int
  | Bool_lit of bool
  | Double_lit of float
  | Char_lit of char
  | String_lit of string
  | List_init of expr list
  | Function_call of string * expr list
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

type func = {
  name : string;
  return_t : types;
  formals : formal list;
  body : stmt list;
}

type actor = {
  fields : field list;
  name : string;
  formals : formal list;
  receive: message_type list;
}
