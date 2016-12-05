open Ast

type sexpr =
    SBinop of sexpr * bin_op * sexpr * types
  | SUnop of u_op * sexpr * types
  | SId of string * types
  | SAssign of string * expr * types
  | Sint_Lit of int
  | SDouble_Lit of float
  | SChar_Lit of char
  | SString_Lit of string
  | SBool_Lit of bool
  | SList_Lit of sexpr list * types
  | SCall of string * sexpr list * types
  | SActor_comm of message * actor_op * actor_type
  | SNoexpr

type sstmt =
    SBlock of stmt list
  | SExpr of sexpr * types
  | SReturn of sexpr * types
  | SIf of sexpr * sstmt * sexpr
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SBreak
  | SContinue

type smessage = {
  name: string;
  formals: formal list;
}

type spattern = {
  message_id: string;
  message_formals: formal list;
  stmts: sstmt list;
}

type sfunc = {
  name: string;
  formals: formal list;
  return_t: types;
  body: sstmt list;
}

type sactor = {
  name: string;
  formals: formal list;
  body: sstmt list;
  functions: sfunc list;
  receive: spattern list;
}

type slambda = {
  formals: formal list;
  return_t: types;
  body: sstmt list;
}

type sprogram = smessage list * sactor list * sfunc list
