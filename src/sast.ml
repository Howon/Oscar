open Ast

type sexpr =
    SInt_Lit     of int
  | SDouble_Lit  of float
  | SChar_Lit    of char
  | SString_Lit  of string
  | SBool_Lit    of bool
  | SUnit_Lit    of unit
  | SList_Lit    of types * sexpr list
  | SSet_Lit     of types * sexpr list
  | SMap_Lit     of types * types * (sexpr * sexpr) list
  | SActor_Lit   of sexpr * (sexpr list)
  | SPool_Lit    of sexpr * (sexpr list) * sexpr
  | SMessage_Lit of sexpr * (sexpr list)
  | SBinop       of sexpr * bin_op * sexpr
  | SUop         of u_op * sexpr
  | SId          of string
  | SLambda      of slambda
  | SCall        of sexpr * (sexpr list)
  | SNoexpr

and sstmt =
    SBlock      of sstmt list
  | SExpr       of sexpr
  | SReturn     of sexpr
  | SVdecl      of sval_decl
  | SMutdecl    of smvar_decl
  | SFdecl      of sfunc
  | SIf         of sexpr * sstmt * sstmt
  | SActor_send of sexpr * sexpr
  | SPool_send  of sexpr * sexpr

and t_expr = sexpr * types

and sval_decl = {
    sv_name : string;
    sv_type : types;
    sv_init : sexpr;
}

and smvar_decl = {
    smv_name : string;
    smv_type : types;
    smv_init : sexpr;
}

and slambda = {
  sl_formals  : formal list;
  sl_return_t : types;
  sl_body     : sstmt;
}

and smessage = {
  sm_name    : string;
  sm_formals : formal list;
}

and spattern = {
  sp_smid      : string;
  sp_smformals : formal list;
  sp_body      : sstmt;
}

and sfunc = {
  sf_name     : string;
  sf_formals  : formal list;
  sf_return_t : types;
  sf_body     : sstmt;
}

and sactor = {
  sa_name      : string;
  sa_formals   : formal list;
  sa_body      : sstmt;
  sa_functions : sfunc list;
  sa_receive   : spattern list;
}

type sprogram = smessage list * sactor list * sfunc list
