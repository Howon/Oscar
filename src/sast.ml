open Ast

type sexpr =
    SInt_Lit of int
  | SDouble_Lit of float
  | SChar_Lit of char
  | SString_Lit of string
  | SBool_Lit of bool
  | SUnit_Lit of unit
  | SList_Lit of types * sexpr list
  | SSet_Lit of types * sexpr list
  | SMap_Lit of types * types * (sexpr * sexpr) list
  | SActor_Lit of string * (sexpr list)
  | SPool_Lit of string * (sexpr list) * int
  | SBinop of sexpr * bin_op * sexpr
  | SUop of u_op * sexpr
  | SId of string
  | SLambda of slambda
  | SCall of string * t_expr list
  | SNoexpr and t_expr = sexpr * types

and sstmt =
    SBlock of stmt list
  | SExpr of sexpr * types
  | SReturn of sexpr * types
  | SVedcl of val_decl
  | SMutdecl of mvar_decl
  | SIf of sexpr * sstmt * sexpr
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SSpawn_act of string * string * string * sexpr list
  | SSpawn_pool of string * string * string * sexpr list
  | SBreak
  | SContinue
  | SActor_send of message * sexpr
  | SPool_send of message * sexpr

and slambda = {
  sl_formals: formal list;
  sl_return_t: types;
  sl_body: sstmt list;
}

and smessage = {
  sm_name: string;
  sm_formals: formal list;
}

and spattern = {
  message_id: string;
  message_formals: formal list;
  stmts: sstmt list;
}

and sfunc = {
  sf_name: string;
  sf_formals: formal list;
  sf_return_t: types;
  sf_body: sstmt list;
}

and sactor = {
  sa_name: string;
  sa_formals: formal list;
  sa_body: sstmt list;
  sa_functions: sfunc list;
  sa_receive: spattern list;
}

type sprogram = smessage list * sactor list * sfunc list
