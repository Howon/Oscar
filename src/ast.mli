type program = None

type bin_op =
    | Add    | Minus | Multiply | Divide
    | Modulo | Equal | Neq      | Lt
    | Leq    | Gt    | Geq      | And
    | Or

type u_op = Not | Neg

type p_type = Int_t | Bool_t | Double_t | Char_t | Unit_t

type cont_type = String_t | List_t | Set_t | Map_t | Tuple_t

type opt_type = Maybe_t of types | Some_t of types | None_t

type message_type = Message_t

type field = Field of types * string

type func = {
    func_name : string;
    function_return_t : types;
    function_formals : formal list;
    function_body : stmt list;
}

type actor = {
    actor_fields : field list;
    actor_name : string;
    acator_receive: receive;
}

type actor_type = Actor_t of string

type pool_type = Pool_t of actor_type list

type types =
    | Primitive of p_type
    | Container of cont_type
    | Optional of opt_type
    | Actor_t of actor_op

type bind_type = types * string

type bit_op =
    | Big_RShift
    | Bit_LShift
    | Bit_And
    | Bit_Or
    | Bit_Xor
    | Bit_Not

type actor_op =
    | Actor_send
    | Actor_broadcast
    | Actor_receive

type expr =
    | Binop of expr * bin_op * expr
    | Bitop of int * bit_op * int
    | Uop of u_op * expr
    | Id of string
    | Assign of expr * expr
    | Int_lit of int
    | Bool_lit of bool
    | Double_lit of float
    | Char_lit of char
    | String_lit of string
    | List_init of expr list
    | Function_call of string * expr list
    | Actor_comm of message_type * actor_op * actor_type
    | Noexpr

type stmt =
    | Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Break
    | Continue
