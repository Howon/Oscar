open Ast
open Sast

let std = "std::"
let immut = "immut::"
let mes = "m_"
let act = "a_"

let actor_include = ""
let immut_include = "#include <oscar/immut.hpp>\n"

let rec c_type (t : types) =
  match t with
      Int_t | Bool_t | Double_t | Char_t -> str_types t
    | String_t           -> std ^ str_types t
    | Unit_t             -> "void"
    | Func_t (args, rt)  -> std ^ "function<" ^ c_type rt ^ "(" ^
                              (String.concat ", " (List.map
                                (fun arg -> c_type arg) args)) ^ ")>"
    | List_t t           -> immut ^ "list<" ^ c_cont_type t
    | Set_t t            -> immut ^ "set<" ^ c_cont_type t
    | Map_t (t1, t2)     -> immut ^ "map<" ^ c_type t1 ^ ", " ^ c_cont_type t2
    | Actor_t t          -> act ^ t
    | Pool_t t           -> "pool<" ^ t ^ ">"
    | Message_t t        -> mes ^ t

and c_cont_type types =
  match types with
      List_t _ | Set_t _ | Map_t (_, _) -> c_type types ^ " >"
    | _ -> c_type types ^ ">"

and init_list (tel : t_expr list) =
    String.concat "," (List.map (fun elem ->
        c_texpr elem
    ) tel)

and init_tup_list (tel : (t_expr * t_expr) list) =
  let c_tup (sk, sv) = str_texpr sk ^ " , " ^ str_texpr sv in
    String.concat "," (List.map (fun elem ->
      "{ " ^ c_tup elem ^ " }"
    ) tel)

and c_texpr tex =
  let (se, _) = tex in
    (match se with
        SInt_Lit _ | SDouble_Lit _ | SChar_Lit _
      | SBool_Lit _ | SId _ | SNoexpr -> str_texpr tex
      | SUnit_Lit _                -> "void"
      | SString_Lit s              -> std ^ "string(\"" ^ s ^ "\")"
      | SAccess (scont, sit)       -> c_texpr scont ^ "[" ^ c_texpr sit ^ "]"
      | SFuncCall (se, sel)        -> se ^ "(" ^ init_list sel ^ ")"
      | SUop (o, se)               -> str_uop o ^ c_texpr se
      | SBinop (se1, o, se2)       -> "(" ^ c_texpr se1 ^ " " ^ str_binop o ^
                                        " " ^ c_texpr se2 ^ ")"
      | SFunc_Lit sfl              -> c_lambda sfl
      | SList_Lit (t, sel)         -> immut ^ "list<" ^ str_cont_t t ^ "{" ^
                                        init_list sel ^ "}"
      | SSet_Lit (t, sel)          -> immut ^ "set<" ^ (str_cont_t t) ^ "{" ^
                                        init_list sel ^ "}"
      | SMap_Lit (kt, vt, skvs)    -> immut ^ "map<" ^ c_type kt ^ ", " ^
                                        str_cont_t vt ^ "{" ^
                                          init_tup_list skvs ^ "}"
      | SActor_Lit (sat, sel)      -> "actor<" ^ act ^ sat ^ ">(" ^
                                        init_list sel ^ ")"
      | SPool_Lit (sat, sel, num)  -> "spawn pool<" ^ act ^ sat ^ ">({" ^
                                        init_list sel ^ "}, " ^
                                          c_texpr num ^ ")"
      | SMessage_Lit (m, sel)      ->  mes ^ m ^ "(" ^
                                        init_list sel ^ ")")

and c_sstmt sstmt =
  match sstmt with
      SBlock _             -> "{\n" ^ c_sstmts sstmt ^ "\n}"
    | SExpr texp           -> c_texpr texp ^ ";"
    | SReturn se           -> "return " ^ (match (snd se) with
                                  Unit_t -> ""
                                | _      -> c_texpr se) ^ ";"
    | SMutdecl smv         -> c_type smv.smv_type ^ " " ^
                                smv.smv_name ^ (match smv.smv_init with
                                    SNoexpr, _ -> ""
                                  | _ -> "=" ^ (c_texpr smv.smv_init)) ^ ";"
    | SVdecl sv            -> "auto " ^ sv.sv_name ^ "=" ^
                                  c_texpr sv.sv_init ^ ";"
    | SIf (se, s1, s2)     -> c_if se s1 s2
    | SActor_send (se, a)  -> c_texpr se ^ " |> " ^ c_texpr a ^ ";"
    | SPool_send (se, p)   -> c_texpr se ^ " |>> " ^ c_texpr p ^ ";"

and c_sstmts = function
    SBlock sstmts   -> String.concat "\n" (List.map c_sstmt sstmts)
  | _           -> ""

and c_formal f = c_type (snd f) ^ " " ^ fst f

and c_formals fs = String.concat "," (List.map c_formal fs)

and c_lambda sfl =
  let { sf_formals = sformals; sf_return_t = srt; sf_body = sbody } = sfl in
    "[&](" ^ c_formals sformals ^ ")" ^ (match srt with
        Unit_t -> ""
      | _ -> "->" ^ c_type srt) ^ c_sstmt sbody;

and c_func vd =
  let sv_name = vd.sv_name and sv_init = vd.sv_init in
    match fst sv_init with
        SFunc_Lit sfl ->
          let { sf_formals; sf_return_t; sf_body } = sfl in
            c_type sf_return_t ^ " " ^ sv_name ^ "(" ^ c_formals sf_formals ^
              ") \n" ^ c_sstmt sf_body ^ "\n"
      | _ ->
          raise (Failure ("Top level function declaration failed: " ^
            sv_name ^ " is not a function"))

and c_if te s1 s2 =
  "if (" ^ (
    let cond = c_texpr te in
    let cond_len = String.length cond in
    if (String.get cond 0 = '(') && (String.get cond (cond_len - 1) = ')') then
      String.sub cond 1 (cond_len - 2)
    else
      cond
    ) ^ ") \n" ^ c_sstmt s1 ^ (match s2 with
      SExpr (SNoexpr, _)  -> "\n"
    | _  -> " else " ^ c_sstmt s2 ^ "\n")

let c_message smessage =
  let { sm_name; sm_formals} = smessage in
    "class m_" ^ sm_name ^ " : message {\n" ^
      "public:\n" ^ "m_" ^ sm_name ^ " (" ^
        (c_formals sm_formals) ^ ") \n};\n"

let c_pattern sp =
  "| " ^ sp.sp_smid ^ "(" ^ str_formals sp.sp_smformals ^ ") => " ^
    str_sstmt sp.sp_body

let c_actor sactor =
  "actor " ^ sactor.sa_name ^ "(" ^ str_formals sactor.sa_formals ^ ") {\n" ^
    str_sstmts sactor.sa_body ^ "\n\n\n\nreceive = {\n" ^ String.concat "\n"
      (List.map c_pattern sactor.sa_receive) ^ "\n}\n}"

and main_decl (se, _) =
  match se with
      SFunc_Lit sfl ->
        let sf_formals = sfl.sf_formals and sf_body = sfl.sf_body in
          "int main (" ^ c_formals sf_formals ^
            ") \n" ^ (
              let sfbody = c_sstmt sf_body in
              let slen = String.length sfbody - 2 in
                String.sub sfbody 0 slen ^ "\nreturn 0;\n}"
            ) ^ "\n"
    | _ -> raise (Failure "Main method not found")

let c_program (smessages, sactors, sfuncs, main) =
  actor_include ^ immut_include ^
    String.concat "\n\n" (List.map c_func sfuncs) ^ "\n" ^
      main_decl main ^ "\n"
