type bin_op =
    Add | Sub | Mult | Div
  | Mod | Equal | Neq | Less
  | Leq | Greater | Geq | And
  | Or | Bit_And | Bit_Or | Bit_Xor
  | Bit_RShift | Bit_LShift

type u_op = Not | Neg

type p_type = Int_t | Bool_t | Float_t | Char_t | Unit_t

type cont_type =
  String_t
  | List_t of types
  | Set_t of types
  | Map_t of types * types
  | Tuple_t of types list

type actor_op =
    Actor_send
  | Actor_broadcast

type types = Primitive of p_type | Container of cont_type

(* Commenting out optional types for now *)
(*type opt_type = Maybe_t | Some_t

type optional_rec = {
    type_: opt_type;
    param: types;
}

type optional = Optional_t of optional_rec | None_t*)

type formal = string * types

type message = {
    name: string;
    formals: formal list;
}

type field = Field of types * string

type actor_type = Actor_t of string

type pool_type = Pool_t of actor_type * int

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
  | Unit_Lit of unit
  | List_Lit of List_t * expr list
  | Set_Lit of Set_t * expr list
  | Map_Lit of Map_t * (expr * expr) list
  | Tuple_Lit of Tuple_t * expr list
  | Call of string * expr list
  | Actor_comm of message * actor_op * actor_type
  | Noexpr

and stmt =
    Block of stt list
  | Expr of expr
  | Return of expr
  | Mut of string * types
  | Vdecl
  | If of expr * stmt * stmt
  | For of int * int * int * stmt
  | While of expr * stmt
  | Break
  | Continue

type pattern = {
  message_id: string;
  message_formals: formal list;
  stmts: stmt list;
}

type func = {
  name: string;
  formals: formal list;
  return_t: types;
  body: stmt list;
}

type actor = {
  name: string;
  formals: formal list;
  body: stmt list;
  functions: func list;
  receive: pattern list;
}

type lambda = {
  formals: formal list;
  return_t: types;
  body: stmt list;
}

type program = message list * actor list * func list
