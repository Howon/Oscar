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
  | SFunc_Lit     of sfunc
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

and sactor = {
  sa_name      : string;
  sa_formals   : formal list;
  sa_body      : sstmt;
  sa_receive   : spattern list;
}

type sprogram = smessage list * sactor list * sval_decl list

(* ========== *)

let str_smessage smessage =
  "message " ^ smessage.sm_name ^ "(" ^ (str_formals smessage.sm_formals) ^ ")"

let rec str_texpr texpr =
  (match (fst texpr) with
    SInt_Lit i                 -> string_of_int i
  | SDouble_Lit f              -> string_of_float f
  | SChar_Lit c                -> "\'" ^ Char.escaped c ^ "\'"
  | SString_Lit s              -> "\"" ^ s ^ "\""
  | SBool_Lit true             -> "true"
  | SBool_Lit false            -> "false"
  | SUnit_Lit u                -> "unit"
  | SId se                     -> se
  | SAccess (scont, sit)       -> str_texpr scont ^ "[" ^ str_texpr sit ^ "]"
  | SFunc_Lit sfl              -> str_sfl sfl

  | SList_Lit (t, sel)         -> "list<" ^ str_cont_t t ^ "[" ^
                                    str_texprs sel ^ "]"
  | SSet_Lit (t, sel)          -> "set<" ^ (str_cont_t t) ^ "[" ^
                                    str_texprs sel ^ "]"
  | SMap_Lit (kt, vt, skvs)    -> "map<" ^ str_types kt ^ ", " ^
                                    str_cont_t vt ^ "[" ^
                                      str_skvs skvs ^ "]"
  | SActor_Lit (sat, sel)      -> "spawn actor<" ^ sat ^ ">(" ^
                                    str_texprs sel ^ ")"
  | SPool_Lit (sat, sel, num)  -> "spawn pool<" ^ sat ^ ">({" ^
                                    str_texprs sel ^ "}, " ^
                                      str_texpr num ^ ")"
  | SMessage_Lit (m, sel)      -> "message<" ^ m ^ ">(" ^
                                    str_texprs sel ^ ")"
  | SBinop (se1, o, se2)       -> "(" ^ str_texpr se1 ^ " " ^ str_binop o
                                      ^ " " ^ str_texpr se2 ^ ")"
  | SUop (o, se)               -> str_uop o ^ str_texpr se
  | SFuncCall (se, sel)        -> se ^ "(" ^ str_texprs sel ^ ")"
  | SNoexpr                    -> ""
  ) (*^ "{{TYPE: "^ str_types (snd texpr) ^ "}}" *)

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
  | SVdecl sv            -> str_svdecl sv
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

and str_sfl sfl =
  let { sf_formals = sformals; sf_return_t = srt; sf_body = sbody } = sfl in
  let s = match sbody with
      SBlock sstmts -> sstmts
    | _ -> [] in

  match (List.length s, List.hd s) with
      (1, SReturn te) ->
        "(" ^ str_formals sformals ^ ") => " ^
        str_types srt ^ " = " ^ str_texpr te
    | _               ->
        "(" ^ str_formals sformals ^ ") => " ^
          str_types srt ^ " = " ^ str_sstmt sfl.sf_body

and str_svdecl sv =
  (match sv.sv_init with
    (SFunc_Lit(sf), _) -> str_sfunc sv.sv_name sf
    | _ ->
        str_types sv.sv_type ^ " " ^ sv.sv_name ^ " = "
        ^ str_texpr sv.sv_init ^ ";")

and str_sfunc name sf =
  let { sf_formals = sformals; sf_return_t = srt; sf_body = sbody } = sf in
  let s = match sbody with
      SBlock sstmts -> sstmts
    | _ -> [] in

  match (List.length s, List.hd s) with
      (1, SReturn te) ->
        "func " ^ name ^ " = (" ^ str_formals sformals ^ ") => " ^
        str_types srt ^ " = " ^ str_texpr te ^ ";"
    | _               ->
        "def " ^ name ^ "(" ^ str_formals sformals ^ ") => " ^
        str_types srt ^ " = " ^ str_sstmt (SBlock s)

and str_spattern sp =
  "| " ^ sp.sp_smid ^ "(" ^ str_formals sp.sp_smformals ^ ") => " ^
    str_sstmt sp.sp_body

let str_sactor sactor =
  "actor " ^ sactor.sa_name ^ "(" ^ str_formals sactor.sa_formals ^ ") {\n" ^
    str_sstmts sactor.sa_body ^ "\n\n\n\nreceive = {\n" ^ String.concat "\n"
      (List.map str_spattern sactor.sa_receive) ^ "\n}\n}"

let str_sprogram (smessages, sactors, sfuncs) =
  String.concat "\n" (List.map str_smessage smessages) ^ "\n\n" ^
    String.concat "\n\n" (List.map str_sactor sactors) ^ "\n\n" ^
      String.concat "\n\n" (List.map str_svdecl sfuncs)
