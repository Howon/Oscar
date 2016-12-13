open Ast

type sexpr =
    SInt_Lit      of int
  | SDouble_Lit   of float
  | SChar_Lit     of char
  | SString_Lit   of string
  | SBool_Lit     of bool
  | SUnit_Lit     of unit
  | SId           of string
  | SAccess       of t_expr * t_expr
  | SLambda       of sfunc
  | SList_Lit     of types * t_expr list
  | SSet_Lit      of types * t_expr list
  | SMap_Lit      of types * types * (t_expr * t_expr) list
  | SActor_Lit    of string * (t_expr list)
  | SPool_Lit     of string * (t_expr list) * t_expr
  | SMessage_Lit  of string * (t_expr list)
  | SBinop        of t_expr * bin_op * t_expr
  | SUop          of u_op * t_expr
  | SFuncCall     of string * (t_expr list)
  | SNoexpr

and sstmt =
    SBlock       of sstmt list
  | SExpr        of t_expr
  | SReturn      of t_expr
  | SVdecl       of sval_decl
  | SMutdecl     of smvar_decl
  | SFdecl       of sfunc_decl
  | SIf          of t_expr * sstmt * sstmt
  | SActor_send  of t_expr * t_expr
  | SPool_send   of t_expr * t_expr

and t_expr = sexpr * types

and sval_decl = {
    sv_name : string;
    sv_type : types;
    sv_init : t_expr;
}

and smvar_decl = {
    smv_name : string;
    smv_type : types;
    smv_init : t_expr;
}

and sfunc = {
  sf_formals  : formal list;
  sf_return_t : types;
  sf_body     : sstmt;
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

and sfunc_decl {
  sf_name     : string;
  sf_func     : sfunc;
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

let rec str_texpr = function
    SInt_Lit i, _                 -> string_of_int i
  | SDouble_Lit f, _              -> string_of_float f
  | SChar_Lit c, _                -> "\'" ^ Char.escaped c ^ "\'"
  | SString_Lit s, _              -> "\"" ^ s ^ "\""
  | SBool_Lit true, _             -> "true"
  | SBool_Lit false, _            -> "false"
  | SUnit_Lit u, _                -> "unit"
  | SId se, _                     -> se
  | SAccess (scont, sit), _       -> str_texpr scont ^ "[" ^ str_texpr sit ^ "]"
  | SLambda slambda, _            -> str_slambda slambda
  | SList_Lit (t, sel), _         -> "list<" ^ str_types t ^ ">[" ^
                                    str_texprs sel ^ "]"
  | SSet_Lit (t, sel), _          -> "set<" ^ (str_types t) ^ ">[" ^
                                    str_texprs sel ^ "]"
  | SMap_Lit (kt, vt, skvs), _    -> "map<" ^ str_types kt ^ ", " ^
                                    str_types vt ^ ">[" ^
                                      str_skvs skvs ^ "]"
  | SActor_Lit (sat, sel), _      -> "spawn actor<" ^ sat ^ ">(" ^
                                    str_texprs sel ^ ")"
  | SPool_Lit (sat, sel, num), _  -> "spawn pool<" ^ sat ^ ">({" ^
                                    str_texprs sel ^ "}, " ^
                                      str_texpr num ^ ")"
  | SMessage_Lit (m, sel), _      -> "message<" ^ m ^ ">(" ^
                                    str_texprs sel ^ ")"
  | SBinop (se1, o, se2), _       -> "(" ^ str_texpr se1 ^ " " ^ str_binop o ^ " " ^
                                    str_texpr se2 ^ ")"
  | SUop (o, se), _               -> str_uop o ^ str_texpr se
  | SFuncCall (se, sel), _        -> se ^ "(" ^ str_texprs sel ^ ")"
  | SNoexpr, _                    -> ""

and str_texprs sel =
  String.concat ", " (List.map str_texpr sel)

and str_skvs skvs =
  let skv_string skv =
    let (sk, sv) = skv in str_texpr sk ^ " -> " ^ str_texpr sv in
  String.concat ", " (List.map skv_string skvs)

and str_sstmt = function
    SBlock(sstmts)       -> "{\n" ^ str_sstmts (SBlock(sstmts)) ^ "\n}"
  | SExpr texp           -> str_texpr texp ^ ";"
  | SReturn(se)          -> "return " ^ str_texpr se ^ ";"
  | SMutdecl smv         -> "mut " ^ (str_types smv.smv_type) ^ " " ^
                              smv.smv_name ^ (match smv.smv_init with
                                  SNoexpr, _ -> ""
                                | _ -> " = " ^ (str_texpr smv.smv_init)) ^ ";"
  | SVdecl sv            -> (str_types sv.sv_type) ^ " " ^ sv.sv_name ^ " = " ^
                              (str_texpr sv.sv_init) ^ ";"
  | SFdecl sf            -> str_sfunc sf
  | SIf (se, s1, s2)     -> str_sif se s1 s2
  | SActor_send (se, a)  -> str_texpr se ^ " |> " ^ str_texpr a ^ ";"
  | SPool_send (se, p)   -> str_texpr se ^ " |>> " ^ str_texpr p ^ ";"

and str_sstmts = function
    SBlock(sstmts)  -> String.concat "\n" (List.map str_sstmt sstmts)
  | _           -> ""

and str_sif se s1 s2 =
  "if (" ^ str_texpr se ^ ") " ^ str_sstmt s1 ^ (match s2 with
      SExpr (SNoexpr, _)  -> ""
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
