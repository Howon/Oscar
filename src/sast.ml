open Ast

type sexpr =
    SInt_Lit      of int
  | SDouble_Lit   of float
  | SChar_Lit     of char
  | SString_Lit   of string
  | SBool_Lit     of bool
  | SUnit_Lit     of unit
  | SId           of string
  | SAccess       of sexpr * sexpr
  | SLambda       of slambda
  | SList_Lit     of types * sexpr list
  | SSet_Lit      of types * sexpr list
  | SMap_Lit      of types * types * (sexpr * sexpr) list
  | SActor_Lit    of sexpr * (sexpr list)
  | SPool_Lit     of sexpr * (sexpr list) * sexpr
  | SMessage_Lit  of sexpr * (sexpr list)
  | SBinop        of sexpr * bin_op * sexpr
  | SUop          of u_op * sexpr
  | SCall         of sexpr * (sexpr list)
  | SNoexpr

and sstmt =
    SBlock       of sstmt list
  | SExpr        of sexpr
  | SReturn      of sexpr
  | SVdecl       of sval_decl
  | SMutdecl     of smvar_decl
  | SFdecl       of sfunc
  | SIf          of sexpr * sstmt * sstmt
  | SActor_send  of sexpr * sexpr
  | SPool_send   of sexpr * sexpr

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
  sa_receive   : spattern list;
}

type sprogram = smessage list * sactor list * sfunc list

(* ========== *)

let str_smessage smessage =
  "message " ^ smessage.sm_name ^ "(" ^ (str_formals smessage.sm_formals) ^ ")"

let rec str_sexpr = function
    SBinop(se1, o, se2)       -> "(" ^ str_sexpr se1 ^ " " ^ str_binop o
                                   ^ " " ^ str_sexpr se2 ^ ")"
  | SUop(o, se)               -> str_uop o ^ str_sexpr se
  | SInt_Lit i                -> string_of_int i
  | SDouble_Lit f             -> string_of_float f
  | SChar_Lit c               -> "\'" ^ Char.escaped c ^ "\'"
  | SString_Lit s             -> "\"" ^ s ^ "\""
  | SBool_Lit true            -> "true"
  | SBool_Lit false           -> "false"
  | SUnit_Lit u               -> "unit"
  | SId se                    -> se
  | SAccess(scont, sit)       -> str_sexpr scont ^ "[" ^ str_sexpr sit ^ "]"
  | SLambda slambda           -> str_slambda slambda
  | SList_Lit(t, sel)         -> "list<" ^ str_types t ^ ">[" ^
                                   str_sexprs sel ^ "]"
  | SSet_Lit(t, sel)          -> "set<" ^ (str_types t) ^ ">[" ^
                                   str_sexprs sel ^ "]"
  | SMap_Lit(kt, vt, skvs)    -> "map<" ^ str_types kt ^ ", " ^
                                   str_types vt ^ ">[" ^
                                     str_skvs skvs ^ "]"
  | SActor_Lit(sat, sel)      -> "spawn actor<" ^ str_sexpr sat ^ ">(" ^
                                   str_sexprs sel ^ ")"
  | SPool_Lit(sat, sel, num)  -> "spawn pool<" ^ str_sexpr sat ^ ">({" ^
                                   str_sexprs sel ^ "}, " ^
                                     str_sexpr num ^ ")"
  | SMessage_Lit(m, sel)      -> "message<" ^ str_sexpr m ^ ">(" ^
                                   str_sexprs sel ^ ")"
  | SCall(se, sel)            -> str_sexpr se ^ "(" ^ str_sexprs sel ^ ")"
  | SNoexpr                   -> ""

and str_sexprs sel =
  String.concat ", " (List.map str_sexpr sel)

and str_skvs skvs =
  let skv_string skv =
    let (sk, sv) = skv in str_sexpr sk ^ " -> " ^ str_sexpr sv in
  String.concat ", " (List.map skv_string skvs)

and str_sstmt = function
    SBlock(sstmts)     -> "{\n" ^ str_sstmts (SBlock(sstmts)) ^ "\n}"
  | SExpr(se)          -> str_sexpr se ^ ";"
  | SReturn(se)        -> "return " ^ str_sexpr se ^ ";"
  | SMutdecl(smv)      -> "mut " ^ (str_types smv.smv_type) ^ " " ^
                            smv.smv_name ^ (match smv.smv_init with
                                SNoexpr -> ""
                              | _ -> " = " ^ (str_sexpr smv.smv_init)) ^ ";"
  | SVdecl(sv)         -> (str_types sv.sv_type) ^ " " ^ sv.sv_name ^ " = " ^
                            (str_sexpr sv.sv_init) ^ ";"
  | SFdecl(sf)         -> str_sfunc sf
  | SIf(se, s1, s2)    -> str_sif se s1 s2
  | SActor_send(se, a) -> (str_sexpr se) ^ " |> " ^ str_sexpr a ^ ";"
  | SPool_send(se, p)  -> (str_sexpr se) ^ " |>> " ^ str_sexpr p ^ ";"

and str_sstmts = function
    SBlock(sstmts)  -> String.concat "\n" (List.map str_sstmt sstmts)
  | _           -> ""

and str_sif se s1 s2 =
  "if (" ^ str_sexpr se ^ ") " ^ str_sstmt s1 ^ (match s2 with
      SExpr(SNoexpr)  -> ""
    | _  -> " else " ^ str_sstmt s2)

and str_slambda slambda =
  "(" ^ str_formals slambda.sl_formals ^ ") => " ^ str_types
    slambda.sl_return_t ^ " = " ^ str_sstmt slambda.sl_body

and str_spattern sp =
  "| " ^ sp.sp_smid ^ "(" ^ str_formals sp.sp_smformals ^ ") => " ^
    str_sstmt sp.sp_body

and str_sfunc sfunc =
  "def " ^ sfunc.sf_name ^ "(" ^ str_formals sfunc.sf_formals ^ ") => " ^
    str_types sfunc.sf_return_t ^ " = " ^ str_sstmt sfunc.sf_body

let str_sactor sactor =
  "actor " ^ sactor.sa_name ^ "(" ^ str_formals sactor.sa_formals ^ ") {\n" ^
    str_sstmts sactor.sa_body ^ "\n\n\n\nreceive = {\n" ^ String.concat "\n"
      (List.map str_spattern sactor.sa_receive) ^ "\n}\n}"

let str_sprogram (smessages, sactors, sfuncs) =
  String.concat "\n" (List.map str_smessage smessages) ^ "\n\n" ^
    String.concat "\n\n" (List.map str_sactor sactors) ^ "\n\n" ^
      String.concat "\n\n" (List.map str_sfunc sfuncs)
