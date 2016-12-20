open Ast
open Sast

open Hashtbl

let a_decls = Hashtbl.create 50
let p_decls = Hashtbl.create 50

let std = "std::"
let immut = "immut::"
let mes = "m_"

let actor_include   = "#include <oscar/actor.hpp>\n"
let message_include = "#include <oscar/message.hpp>\n"
let immut_include   = "#include <oscar/immut.hpp>\n"

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
    | Actor_t t          -> "a_" ^ t
    | Pool_t t           -> "pool<" ^ t ^ " *>"
    | Message_t t        -> mes ^ t

and c_cont_type types =
  match types with
      List_t _ | Set_t _ | Map_t (_, _) -> c_type types ^ " >"
    | _ -> c_type types ^ ">"

and init_list (tel : t_expr list) a =
    String.concat ", " (List.map (fun elem ->
        c_texpr elem a
    ) tel)

and init_tup_list (tel : (t_expr * t_expr) list) =
  let c_tup (sk, sv) = str_texpr sk ^ " , " ^ str_texpr sv in
    String.concat "," (List.map (fun elem ->
      "{ " ^ c_tup elem ^ " }"
    ) tel)

and c_texpr tex (a : bool) =
  let (se, _) = tex in
    (match se with
        SInt_Lit _ | SDouble_Lit _ | SChar_Lit _
      | SBool_Lit _ | SId _ | SNoexpr -> str_texpr tex
      | SUnit_Lit _                -> "void"
      | SString_Lit s              -> std ^ "string(\"" ^ s ^ "\")"
      | SAccess (scont, sit)       -> c_texpr scont a ^ "[" ^ c_texpr sit a ^ "]"
      | SFuncCall (se, sel)        -> se ^ "(" ^ init_list sel a ^ ")"
      | SUop (o, se)               -> str_uop o ^ c_texpr se a
      | SBinop (se1, o, se2)       -> "(" ^ c_texpr se1 a ^ " " ^
                                        str_binop o ^ " " ^ c_texpr se2 a ^ ")"
      | SFunc_Lit sfl              -> c_lambda sfl a
      | SList_Lit (t, sel)         -> immut ^ "list<" ^ str_cont_t t ^ "{" ^
                                        init_list sel a ^ "}"
      | SSet_Lit (t, sel)          -> immut ^ "set<" ^ (str_cont_t t) ^ "{" ^
                                        init_list sel a ^ "}"
      | SMap_Lit (kt, vt, skvs)    -> immut ^ "map<" ^ c_type kt ^ ", " ^
                                        str_cont_t vt ^ "{" ^
                                          init_tup_list skvs ^ "}"
      | SActor_Lit (sat, sel)      -> "new a_" ^ sat ^ "(" ^
                                        init_list sel a ^ ")"
      | SPool_Lit (sat, sel, num)  -> "new pool<a_" ^ sat ^ ">({" ^
                                        init_list sel a ^ "}, " ^
                                          c_texpr num a ^ ")"
      | SMessage_Lit (m, sel)      ->  "new " ^ mes ^ m ^ "(" ^
                                         (init_list sel a) ^
                                           (if List.length sel = 0 then
                                             "" else ", ") ^ (if a then "this"
                                         else "NULL") ^ ")")

and c_sstmt sstmt (a : bool) =
  match sstmt with
      SBlock _             -> "{\n" ^ c_sstmts sstmt a ^ "\n}"
    | SExpr texp           -> c_texpr texp a ^ ";"
    | SReturn se           -> "return " ^ (match (snd se) with
                                  Unit_t -> ""
                                | _      -> c_texpr se a) ^ ";"
    | SMutdecl smv         -> c_type smv.smv_type ^ " " ^
                                smv.smv_name ^ (match smv.smv_init with
                                    SNoexpr, _ -> ""
                                  | _ -> "=" ^ (c_texpr smv.smv_init a)) ^ ";"

    | SVdecl sv            -> if a then
                                c_func sv
                              else
                                let () = (match sv.sv_type with
                                  Actor_t _ ->
                                    Hashtbl.replace a_decls sv.sv_name true
                                | Pool_t _ ->
                                    Hashtbl.replace p_decls sv.sv_name true
                                | _ ->  ()
                                ) in
                                "auto " ^ sv.sv_name ^ "=" ^
                                  c_texpr sv.sv_init a ^ ";"
    | SIf (se, s1, s2)     -> c_if se s1 s2 a
    | SActor_send (se, act)  ->
        let a_name = (match fst act with
            SId s -> s
          | _ -> raise (Failure ("sent to not ID (should not happen)"))
        ) in
        let () = (match snd se with
            Message_t "die" -> Hashtbl.remove a_decls a_name
          | _ -> ()
        ) in
        (match a_name with
            "sender"  ->
              "theMsgThatWasReceived->sender->receive(" ^ c_texpr se a ^ ");\n"
          | _             ->
              c_texpr act a ^ "->receive(" ^ c_texpr se a ^ ");\n")
    | SPool_send (se, p)  ->
        let p_name = (match fst p with
            SId s -> s
          | _ -> raise (Failure ("sent to not ID (should not happen)"))
        ) in
        let () = (match snd se with
            Message_t "die" -> Hashtbl.remove p_decls p_name
          | _ -> ()
        ) in
        c_texpr p a ^ "->broadcast(" ^ c_texpr se a ^ ");\n"

and c_sstmts sstmts a =
  (match sstmts with
    SBlock sstmts   -> String.concat "\n" (List.map (fun s ->
      c_sstmt s a) sstmts)
  | _           -> "")

and c_formal f = c_type (snd f) ^ " " ^ (match snd f with
    Actor_t _ -> "*"
  | _ -> "") ^ fst f

and c_formals fs = String.concat "," (List.map c_formal fs)

and c_lambda sfl a =
  let { sf_formals = sformals; sf_return_t = srt; sf_body = sbody } = sfl in
    "[&](" ^ c_formals sformals ^ ")" ^ (match srt with
        Unit_t -> ""
      | _ -> "->" ^ c_type srt) ^ c_sstmt sbody a;

and c_func vd =
  let sv_name = vd.sv_name and sv_init = vd.sv_init in
    match fst sv_init with
        SFunc_Lit sfl ->
          let { sf_formals; sf_return_t; sf_body } = sfl in
            c_type sf_return_t ^ " " ^ sv_name ^ "(" ^ c_formals sf_formals ^
              ")\n" ^ c_sstmt sf_body false ^ "\n"
      | _ ->
          raise (Failure ("Top level function declaration failed: " ^
            sv_name ^ " is not a function"))

and c_if te s1 s2 a =
  "if (" ^ (
    let cond = c_texpr te a in
    let cond_len = String.length cond in
      if (String.get cond 0 = '(') &&
        (String.get cond (cond_len - 1) = ')') then
        String.sub cond 1 (cond_len - 2)
      else
        cond
      ) ^ ")\n" ^ c_sstmt s1 a ^ (match s2 with
          SExpr (SNoexpr, _)  -> "\n"
        | _  -> " else " ^ c_sstmt s2 a ^ "\n"
      )

let declare_fields formals =
  String.concat ";\n" (List.map c_formal formals) ^
    if List.length formals > 0 then ";\n" else ""

let constructor_assignment formals =
  String.concat ";\n" (List.map (fun sf ->
    "this->" ^ fst sf ^ " = " ^ fst sf
  ) formals) ^ if List.length formals > 0 then ";\n" else ""

let get_mbody formals =
  "tuple<" ^ String.concat ", " (List.map (fun sf ->
    c_type (snd sf) ) formals) ^ "> get() {\nreturn make_tuple(" ^
      (String.concat ", " (List.map (fun sf -> "this->" ^ fst sf ) formals)) ^
        ");\n}"

let c_message smessage =
  let { sm_name; sm_formals} = smessage in
    "class m_" ^ sm_name ^ " : public Message {\nprivate:\n" ^
      declare_fields sm_formals ^ "\npublic:\nm_" ^ sm_name ^ " (" ^
        c_formals sm_formals ^ (if List.length sm_formals = 0 then "" else ",") ^
          "Actor *sender) : Message(\"" ^ sm_name ^ "\", sender)\n{\n" ^
            constructor_assignment sm_formals ^ "\n}\n" ^
              get_mbody sm_formals ^ "\n};\n"

let declare_queues formals =
  String.concat ";\n" (List.map (fun m ->
    "std::queue<m_" ^ m.sm_name ^ " *> " ^ m.sm_name ^ "Queue"
  ) formals) ^ if List.length formals > 0 then ";\n" else ""

let cast_message message =
  let { sm_name = sm_name ; sm_formals = _} = message in
    "if (m_" ^ sm_name ^ " *pm = dynamic_cast<m_" ^ sm_name ^ " *>(msg)) {\n" ^
      "unique_lock<mutex> lck(mx);\n" ^ sm_name ^ "Queue.push(pm);\n" ^
        "goto notify;\n}\n"

let unpack_body body = String.sub body 1 (String.length body - 2)

(* we give this theMsgThatWasReceived a name so long to avoid conflict *)
let c_pattern sp =
  let { sp_smid = sp_smid; sp_smformals = sp_smformals; sp_body = _ } = sp in
    "void respond(m_" ^ sp_smid ^ " *theMsgThatWasReceived) {\n"
    ^ String.concat ";\n"
      (List.mapi (fun i f ->
        "auto " ^ fst f ^ " = get<" ^ string_of_int i ^
        ">(theMsgThatWasReceived->get())"
      ) sp_smformals) ^ (if List.length sp_smformals > 0 then ";\n" else "") ^ (
        let actor_body = c_sstmt sp.sp_body true in
          unpack_body actor_body) ^ "delete theMsgThatWasReceived;\n" ^
      (if sp_smid = "die" then "Die();\n" else "") ^ "}\n"

let consume messages =
    "void consume() {\nunique_lock<mutex> lck(mx);\nwhile (!this->tFinished)" ^
      "{\nwhile (" ^ (String.concat "&&" (List.map (fun m ->
           m.sm_name ^ "Queue.empty()"
         ) messages)) ^ "){\nif (tFinished)\nreturn;\ncv.wait(lck);\n}\n" ^
        (String.concat "\n" (List.map (fun m ->
          "if (!" ^ m.sm_name ^ "Queue.empty()) {\n" ^
            "auto msg = " ^ m.sm_name ^ "Queue.front();\n" ^
              m.sm_name ^ "Queue.pop();\nrespond(msg);\ngoto loop;\n}"
        ) messages)) ^ "\nloop: ;\n}\n}"

let make_die _ =
  "\nvoid Die() {\nthis->tFinished = true;\nt.join();\n}\n"

let c_actor sactor_scope =
  let sactor = sactor_scope.a_actor in
  let smessages = sactor_scope.a_messages in
  let { sa_name; sa_formals; sa_body; sa_receive } = sactor in
    "class a_" ^ sactor.sa_name ^ " : public Actor {\nprivate:\n" ^
      declare_fields sa_formals ^ declare_queues smessages ^
        unpack_body (c_sstmt sa_body true) ^ "\n\npublic:\n" ^ "a_" ^ sa_name ^
          " (" ^ (c_formals sa_formals) ^ ") : Actor()\n{\n" ^
            constructor_assignment sa_formals ^ "\n" ^
              "t = thread([=] { consume(); });\n}\nvirtual ~a_" ^ sa_name ^
              "() {\nDie();\n}\n" ^ (make_die ()) ^
              "virtual void receive(Message *msg) {\n" ^
              String.concat "" (List.map cast_message smessages) ^
              "notify:\ncv.notify_one();\n}\n" ^ String.concat "\n"
              (List.map c_pattern sa_receive) ^ consume smessages ^ "};\n"

let die_pools_actors s=
  let f_kill k _ acc = "delete " ^ k ^";\n" ^ acc in
  let die_pools = Hashtbl.fold f_kill p_decls s in
  Hashtbl.fold f_kill a_decls die_pools

let main_decl (se, _) =
  match se with
      SFunc_Lit sfl ->
        let sf_formals = sfl.sf_formals and sf_body = sfl.sf_body in
          "int main (" ^ c_formals sf_formals ^
            ") " ^ (
              let sfbody = c_sstmt sf_body false in
              let slen = String.length sfbody - 2 in
              String.sub sfbody 0 slen ^ "\nreturn 0;\n}"
            ) ^ "\n"
    | _ -> raise (Failure "Main method not found")

let c_program (smessages, sactors, sfuncs, main) =
  actor_include ^ immut_include ^
  String.concat "\n" (List.map c_message smessages) ^ "\n\n" ^
  String.concat "\n" (List.map c_actor sactors) ^ "\n" ^
  String.concat "\n\n" (List.map c_func sfuncs) ^ "\n" ^
    main_decl main ^ "\n"
