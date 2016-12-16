open Ast
open Sast

let rec c_type (t : types) =
  match t with
      Int_t | Bool_t | Double_t | Char_t | String_t -> str_types t
    | Unit_t               -> "void"
    | Lambda_t (args, rt)  -> "lambda (" ^ (String.concat ", " (List.map
                                (fun arg -> c_type arg) args)) ^ ") => " ^
                                  c_type rt
    | List_t t             -> "list<" ^ str_cont_ct t
    | Set_t t              -> "set<" ^ str_cont_ct t
    | Map_t (t1, t2)       -> "map<" ^ c_type t1 ^ ", " ^ str_cont_ct t2
    | Actor_t t            -> "actor<" ^ t ^ ">"
    | Pool_t t             -> "pool<" ^ t ^ ">"
    | Message_t t          -> "message<" ^ t ^ ">"

and str_cont_ct types =
  match types with
      List_t _ | Set_t _ | Map_t (_, _) -> c_type types ^ " >"
    | _ -> c_type types ^ ">"

let rec c_texpr tex =
  let init_list (tel : t_expr list) (is_map : bool) =
    String.concat "," (List.map (fun elem ->
      if is_map then
        "{" ^ c_texpr (fst elem) ^ "," ^ c_texpr (snd elem) ^ "}"
      else
        c_texpr elem
    )) in

  let (se, t) = tex in
    (match se with
        SInt_Lit _ | SDouble_Lit _ | SChar_Lit _ | SString_Lit _ |
        SBool_Lit _ | SId _ | SAccess (_, _) | SBinop (_, _, _) |
        SUop (_, _) | SFuncCall (_, _) | SNoexpr -> c_texpr tex
      | SUnit_Lit u                -> "unit"
      | SLambda slambda            -> str_slambda slambda
      | SList_Lit (t, sel)         -> "list<" ^ str_cont_t t ^ "{" ^
                                        init_list sel false ^ "}"
      | SSet_Lit (t, sel)          -> "set<" ^ (str_cont_t t) ^ "{" ^
                                        init_list sel false ^ "}"
      | SMap_Lit (kt, vt, skvs)    -> "map<" ^ c_type kt ^ ", " ^
                                        str_cont_t vt ^ "{" ^
                                          init_list skvs true ^ "}"
      | SActor_Lit (sat, sel)      -> "spawn actor<" ^ sat ^ ">(" ^
                                        init_list sel false ^ ")"
      | SPool_Lit (sat, sel, num)  -> "spawn pool<" ^ sat ^ ">({" ^
                                        init_list sel false ^ "}, " ^
                                          c_texpr num ^ ")"
      | SMessage_Lit (m, sel)      -> "message<" ^ m ^ ">(" ^
                                        init_list sel false ^ ")")

and c_sstmt sstmt =
  match sstmt with
      SBlock sstmts        -> "{\n" ^ str_sstmts (SBlock(sstmts)) ^ "\n}"
    | SExpr texp           -> c_texpr texp ^ ";"
    | SReturn se           -> "return " ^ c_texpr se ^ ";"
    | SMutdecl smv         -> "mut " ^ (str_types smv.smv_type) ^ " " ^
                                smv.smv_name ^ (match smv.smv_init with
                                    SNoexpr, _ -> ""
                                  | _ -> " = " ^ (c_texpr smv.smv_init)) ^ ";"
    | SVdecl sv            -> (str_types sv.sv_type) ^ " " ^ sv.sv_name ^ " = " ^
                                (c_texpr sv.sv_init) ^ ";"
    | SFdecl sf            -> str_sfunc sf
    | SIf (se, s1, s2)     -> str_sif se s1 s2
    | SActor_send (se, a)  -> c_texpr se ^ " |> " ^ c_texpr a ^ ";"
    | SPool_send (se, p)   -> c_texpr se ^ " |>> " ^ c_texpr p ^ ";"


