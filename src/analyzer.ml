open Ast
open Sast
open Exception
open Prettyprint

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type vsymtab = {
  vparent      : vsymtab option;
  svals        : sval_decl list
}

type mvsymtab = {
  mvparent     : mvsymtab option;
  smvars       : smvar_decl list
}

type actor_scope = {
  a_actor    : sactor;
  a_scope    : scope;
  a_messages : smessages list;
} and scope = {
  messages    : smessage list;
  actors      : actor_scope list;
  funcs       : sfunc list;
  env_vtable  : vsymtab;
  env_mvtable : mvsymtab;
  return_t    : types option;
  in_actor    : bool;
}

let rec find_value_decl (vstab : vsymtab) (v_name : string) =
  try
    List.find (fun sval -> sval.sv_name = v_name) vstab.svals
  with Not_found ->
    match vstab.vparent with
        Some(vparent) -> find_value_decl vparent v_name
      | _ -> raise Not_found

let rec find_variable_decl (mvstab : mvsymtab) (mv_name : string) =
  try
    List.find (fun mvar -> mvar.smv_name = mv_name) mvstab.smvars
  with Not_found ->
    match mvstab.mvparent with
        Some mvparent -> find_variable_decl mvparent mv_name
      | _ -> raise Not_found

let is_immu (vstab : vsymtab) (v_name : string) =
  try
    let _ = find_value_decl vstab v_name in true
  with
     Not_found -> false

let is_mut (mvstab: mvsymtab) (mv_name : string) =
  try
    let _ = find_variable_decl mvstab mv_name in true
  with
    Not_found -> false

let find_vtype (vstab : vsymtab) (mvstab : mvsymtab) (name : string) =
  try
    let vtype = find_value_decl vstab name in
    try
      let mvtype = find_variable_decl mvstab name in mvtype.mv_type
    with
        Not_found -> vtype.v_type
      | _ -> raise Exception.Duplicate_decl
  with Not_found ->
    try
      let mvtype = find_variable_decl mvstab name in mvtype.mv_type
    with Not_found -> raise Not_found

let find_message (sm_name : string) (env : scope) =
  try
    List.find (fun sm -> sm_name = sm.sm_name) env.messages
  with Not_found ->
    raise Failure("Message of type " ^ sm_name ^ " not found")

let find_actor (sa_name : string) (env : scope) =
  try
    List.find (fun a_scope -> sa_name = a_scope.a_actor.sa_name) env.actors
  with Not_found ->
    raise Failure("Actor of type " ^ sa_name ^ " not found")

let find_func (sf_name : string) (env : scope)  =
  try
    List.find (fun sfun -> sf_name = sfun.sf_name) env.funcs
  with Not_found ->
    raise Failure("Function of name " ^ sf_name ^ " not found")

let get_comp_texpr (op : bin_op) (e1 : sexpr) (t1 : types)
    (e2 : sexpr) (t2 : types) =
  match t1, t2 with
      Int_t, Int_t       -> (SBinop(e1, op, e2), Int_t)
    | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
    | Char_t, Char_t     -> (SBinop(e1, op, e2), Char_t)
    | String_t, String_t -> (SBinop(e1, op, e2), String_t)
    | Bool_t, Bool_t     -> (SBinop(e1, op, e2), Bool_t)
    | List_t(lt1), List_t(lt2) when lt1 = lt2 ->
        (SBinop(e1, op, e2), List_t(lt1))
    | Map_t(kt1, vt1), Map_t(kt2, vt2) when (kt1 = kt2) && (vt1 = vt2) ->
        (SBinop(e1, op, e2), Map_t(kt1, vt1))
    | Set_t(st1), Set_t(st2) when st1 = st2 -> (SBinop(e1, op, e2), Set_t(st1))
    | Actor_t(_), Actor_t(_) -> raise Exception.Actor_err
    | Pool_t(_), Pool_t(_)   -> raise Exception.Pool_err
    | _ -> raise Failure("operand type mismatch: " ^
                      (Prettyprint.str_types t1) ^ " " ^
                      (Prettyprint.str_types t2))

let check_binop (te1 : t_expr) (te2 : t_expr)
    (op : bin_op) (env : scope) =
  let (e1, t1) = te1 and (e2, t2) = te2 in
  match op with
      Add -> (match t1, t2 with
          Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
        | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
        | String_t, String_t -> (SBinop(e1, op, e2), String_t)
        | _ -> raise Failure("operand type mismatch: " ^
                          (str_types t1) ^ " " ^ (str_types t2)))
    | Sub | Mult | Div | Less | Leq | Greater | Geq -> (match t1, t2 with
          Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
        | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
        | _ -> raise Failure("operand type mismatch: " ^
                          (str_types t1) ^ " " ^ (str_types t2)))
    | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
        (match t1, t2 with
            Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
          | _ -> raise Failure("operand type mismatch: " ^
                          (str_types t1) ^ " " ^ (str_types t2)))
    | Equal | Neq ->
        (try
          get_comp_texpr op e1 t1 e2 t2
        with
            Exception.Actor_err ->
              raise (Failure "Actors cannot be compared for equality")
          | Exception.Pool_err  ->
              raise (Failure "Pools cannot be compared for equality"))
    | And | Or -> (match t1, t2 with
          Bool_t, Bool_t -> (SBinop(e1, op, e2), Bool_t)
        | _ -> raise Failure("operand type mismatch: " ^
                          (str_types t1) ^ " " ^ (str_types t2)))
    | Assign -> (match e1 with
          SId s when t1 = t2 ->
            (if (is_immu env.env_vtable s) then
              raise Failure("Reassignment to a value " ^ s)
            else
              if (is_mut env.env_mvtable s) then
                (try
                    get_comp_texpr op e1 t1 e2 t2
                with
                    Exception.Actor_err | Exception.Pool_err ->
                      if t1 = t2 then
                        (SBinop(e1, op, e2), t1)
                      else
                        raise (Failure "Assignment to incompatible type"))
              else
                raise (Failure "Assignment to invalid id" ))
        | _ -> raise (Failure "Assignment to incompatible type" ))
    | Access -> (match t1, t2 with
          List_t(lt), Int_t             -> (SBinop(e1, op, e2), lt)
        | Map_t(kt, vt), _ when kt = t2 -> (SBinop(e1, op, e2), vt)
        | Set_t(st), _ when st = t2     -> (SBinop(e1, op, e2), Bool_t)
        | _ -> raise (Failure "Invalid access types" ))

let check_uop (te : t_expr) (op : u_op) =
  let (e, t) = te in match op with
      Neg -> (match t with
          Int_t    -> (SUop(op, e), Int_t)
        | Double_t -> (SUop(op, e), Double_t)
        | _ -> raise Failure("operand type mismatch: " ^
                          Prettyprint.str_uop op ^ " on " ^ Prettyprint.str_expr e))
    | Not -> (match t with
          Bool_t -> (SUop(op, e), Bool_t)
        | _ -> raise Failure("operand type mismatch: " ^
                          Prettyprint.str_uop op ^ " on " ^ Prettyprint.str_expr e))

let check_args_t (params : types list) (args : types list) =
  try
    List.iter2 (fun t1 t2 -> (match t1, t2 with
        Lambda_t(args1, rt1), Lambda_t(args2, rt2)
           when (rt1 != rt2) || (args1 <> args2) ->
             raise Exception.Type_mismatch
      | _ ->
           if (t1 != t2) then raise
             Exception.Type_mismatch
           else ()
    )) params args;
    true
  with Exception.Type_mismatch -> false

let check_args (params : types list) (args : t_expr list) =
  let actual_args = List.map (fun (_, x) -> x) args in
  check_args_t params actual_args

let texpr_to_sexpr_list (ltexpr : t_expr list) =
  List.map (fun (x, _) -> x) ltexpr

let check_message_lit (sm: smessage) (args : t_expr list) =
  let params = List.map (fun (_, x) -> x) sm.sm_formals in
  if check_args params args then
    let sargs = texpr_to_sexpr_list args in
    (SMessage_Lit(SId(sm.sm_name), sargs), Message_t(Id(sm.sm_name)))
  else
    raise (Failure "Message constructed with wrong parameter types")

let check_actor_lit (sa : sactor) (args : t_expr list) =
  let params = List.map (fun (_, x) -> x) sa.sa_formals in
  if check_args params args then
    let sargs = texpr_to_sexpr_list args in
    (SActor_Lit(SId(sa.sa_name), sargs), Actor_t(Id(sa.sa_name)))
  else
    raise (Failure "Actor constructed with wrong parameter types")

let check_func_call (sf : sfunc) (args : t_expr list) =
  let params = List.map (fun (_, x) -> x) sf.sf_formals in
  if check_args params args then
    let sargs = texpr_to_sexpr_list args in
    (SCall(SId(sf.sf_name), sargs), sf.sf_return_t)
  else
    raise (Failure "Function called with conflicting types")

let rec check_expr (e : expr) (env : scope) =
  match e with
      Int_Lit i           -> (SInt_Lit i, Int_t)
    | Double_Lit d        -> (SDouble_Lit d, Double_t)
    | Char_Lit c          -> (SChar_Lit c, Char_t)
    | String_Lit s        -> (SString_Lit s, String_t)
    | Bool_Lit b          -> (SBool_Lit b, Bool_t)
    | Unit_Lit u          -> (SUnit_Lit u, Unit_t)
    | List_Lit(lt, ex) ->
        let t_expr_list = List.map (fun t -> check_expr t env) ex in
        let sexpr_list = List.map (fun (se, _) -> se) t_expr_list in
        (SList_Lit(lt, sexpr_list), List_t lt)
    | Set_Lit(st, ex) ->
        let t_expr_list = List.map (fun t -> check_expr t env) ex in
        let sexpr_list = List.map (fun (se, _) -> se) t_expr_list in
        (SSet_Lit(st, sexpr_list), Set_t st)
    | Map_Lit(kt, vt, kvx) ->
        let tkv_list= List.map (fun (k, v) ->
          ((check_expr k env), (check_expr v env))
        ) kvx in
        let skv_list = List.map (fun ((k, _), (t, _)) -> (k, t)) tkv_list in
        (SMap_Lit(kt, vt, skv_list), Map_t(kt, vt))
    | Actor_Lit(at, e) ->
        let (act_t, _) = check_expr at env in
          (match act_t with
            SId(act_name) ->
              let sactor = find_actor act_name env in
              let te_list = List.map (fun exp -> check_expr exp env) e in
              check_actor_lit sactor te_list
          | _ -> raise (Failure "Invalid actor type"))
    | Pool_Lit(at, e, num) ->
        let (sa_num, num_act) = check_expr num env in
        (match sa_num with
            SInt_Lit(x)->
              if x < 1 then
                raise (Failure "Number of actors in a pool must be at least 1")
              else
                let (act_t, _) = check_expr at env in
                (match act_t with
                    SId(act_name) ->
                      let sactor = find_actor act_name env in
                      let te_list = List.map (fun exp -> check_expr exp env) e in
                      (match check_actor_lit sactor te_list with
                          SActor_Lit(sa_id, sa_args), Actor_t(a_t) ->
                            (SPool_Lit(sa_id, sa_args, sa_num), Pool_t(a_t))
                        | _ -> raise (Failure "Pool construction failed"))
                  | _ -> raise (Failure "Invalid actor type in this pool"))
          | _ -> raise (Failure ("Number of actors must be an integer")))
    | Message_Lit(m, e) ->
        let (m_t, _) = check_expr m env in
        (match m_t with
          SId(m_name) ->
            let smessage = find_message m_name env in
            let te_list = List.map (fun exp -> check_expr exp env) e in
            check_message_lit smessage te_list
        | _ -> raise (Failure "Invalid message type"))
    | Binop(e1, op, e2) ->
        let checked_e1 = check_expr e1 env and
        checked_e2 = check_expr e2 env in
        check_binop checked_e1 checked_e2 op env
    | Uop(op, e) ->
        let checked_e = check_expr e env in check_uop checked_e op
    | Id id ->
        (try
          let v_t = find_vtype env.env_vtable env.env_mvtable id in
          (SId id, v_t)
        with
            Not_found ->
              raise (Failure ("Undeclared identifier " ^ id))
          | Exception.Duplicate_decl ->
              raise (Failure (id ^ " declared as both mutable and immutable")))
    | Lambda ld ->
        let formal_types = List.map (fun (_, ft) -> ft) ld.l_formals in
        (SLambda {
          sl_formals  = ld.l_formals;
          sl_return_t = ld.l_return_t;
          sl_body     = fst (check_stmt ld.l_body env)
        }, Lambda_t (formal_types, ld.l_return_t))
    | Call(f, args) ->
        let (et, _) = check_expr f env in
        (match et with
            SId f_id ->
              let sf = find_func f_id env in
              let tex_args = List.map (fun exp -> check_expr exp env) args in
              check_func_call sf tex_args
          | _ -> raise (Failure ("Invalid function id " ^ str_expr f)))
     | Noexpr -> (SNoexpr, Unit_t)

and check_stmt (s : stmt) (env : scope) =
  match s with
      Block sl ->
        let nvsymt = { vparent = Some env.env_vtable; svals = []; } in
        let nmv_symt = { mvparent = Some env.env_mvtable; smvars  = []; } in
        let nenv = { env with env_vtable = nvsymt; env_mvtable = nmv_symt; } in
        let (checked_stmts, _) = check_stmt_list sl false nenv in
        (SBlock checked_stmts, env)
    | Expr e -> (SExpr (fst (check_expr e env)), env)
    | Return e ->
        let (sexpr, t) = check_expr e env in
        (match env.return_t with
            Some rt ->
              if (t != rt) then
                (SReturn (sexpr), env)
              else
                raise (Failure "Return type mismatch")
          | _ -> raise (Failure "This function does not return"))
    | Vdecl vdecl -> check_vdecl vdecl env
    | Mutdecl mvdecl -> check_mvdecl mvdecl env
    | Fdecl fdecl ->
        check_func_decl fdecl
    | If(cond, isl, esl) ->
        let (se, t) = check_expr cond env in
        (match t with
            Bool_t ->
              let (check_if, _) = check_stmt isl env in
              let (check_esl, _) = check_stmt esl env in
              (SIf(se, check_if, check_esl), env)
          | _ -> raise (Failure "Condition must be a boolean"))
    | Actor_send(e1, e2)->
        let (se1, st1) = check_expr e1 env in
        let (se2, st2) = check_expr e2 env in
        (match st1, st2 with
            Message_t(Id m_name), Actor_t(Id a_name) ->
              let sm = find_message m_name env in
              let sm_formal = List.map (fun (_, t) -> t) sm.sm_formals in
              let sm_allowed = (find_actor a_name env).a_messages in
              (try
                let _ = List.find (fun m -> m.m_name = sm.sm_name) sm_allowed in
                (SActor_send(se1, se2), env)
              with Not_found ->
                raise (Failure ("No matching pattern to receive message " ^
                           m_name ^ " in actor " ^ a_name)))
          | _ -> raise (Failure ("Invalid send operation between " ^
                            Prettyprint.str_types st1 ^ " to " ^
                            Prettyprint.str_types st2)))
    | Pool_send(e1, e2) ->
        let (se1, st1) = check_expr e1 env in
        let (se2, st2) = check_expr e2 env in
        (match st1, st2 with
            Message_t(Id m_name), Pool_t(Id a_name) ->
              let sm = find_message m_name env in
              let sm_formal = List.map (fun (_, t) -> t) sm.sm_formals in
              let spattern = (find_actor a_name env).sa_receive in
              let sm_allowed = (find_actor a_name env).a_messages in
              (try
                let _ = List.find (fun m -> m.m_name = sm.sm_name) sm_allowed in
                (SActor_send(se1, se2), env)
              with Not_found ->
                raise (Failure ("No matching pattern to receive message " ^
                           m_name ^ " in actor " ^ a_name)))
          | _ -> raise (Failure ("Invalid send operation between " ^
                            Prettyprint.str_types st1 ^ " to " ^
                            Prettyprint.str_types st2))) and

check_vdecl (vdecl : val_decl) (env : scope) =
  let {v_name; v_type; v_init} = vdecl in
  (match v_type with
      Lambda_t(_, _) -> raise (Failure "Cannot declare lambda types")
    | _ ->
      let (se, t) = check_expr v_init env in
      if v_type = t then
        (try
          let _ = (List.find (fun sval ->
            sval.sv_name = v_name
          ) env.env_vtable.svals) in
          raise (Failure ("Immutable val " ^ v_name ^ " declared already"))
        with Not_found ->
          let svdecl = sval_decl(v_name, v_type, se) in
          let nvsymbt = {
            env.env_vtable with svals = svdecl :: env.env_vtable.svals;
          } in let nenv = { env with
            env_vtable = nvsymbt
          } in (SVdecl svdecl), nenv)
      else
        raise (Failure ("Value initialization type mismatch " ^
                   (str_types vdecl.v_type) ^ " " ^ (str_types t)))) and

check_mvdecl (mvdecl : mvar_decl) (env : scope) =
  if env.in_actor then
    let {mv_name; mv_type; mv_init} = mvdecl in
    (match mv_type with
        Lambda_t(_, _) -> raise (Failure "Cannot declare lambda types")
      | _ ->
        let (se, t) = check_expr mv_init env in
        if mv_type = t then
          (try
            let _ = (List.find (fun smvar ->
              smvar.smv_name = mv_name
            ) env.env_mvtable.smvars) in
            raise Failure("Mutable var " ^ mv_name ^ " declared already")
          with Not_found ->
            let smvdecl = smvar_decl(mv_name, mv_type, se) in
            let nmvsymbt = {
              env.env_mvtable with smvars = smvdecl :: env.env_mvtable.smvars;
            } in let nenv = { env with
              env_mvtable = nmvsymbt
            } in (SMutdecl smvdecl), nenv)
        else
          raise Failure("Variable initialization type mismatch " ^
                     (str_types mvdecl.mv_type) ^ " " ^ (str_types t)))
  else
    raise Failure("Mutables types are only allowed in actors") and

check_func_decl (fdecl : func) (env : scope) =
  let {fname; fformals; frt; fbody} = fdecl in
  (try
    let _ = List.find (fun f -> f.f_name = fname ) env in
    raise (Failure ("Function " ^ fdecl.f_name ^ " declared already"))
  with Not_found ->
    let nvsymtab = List.fold_left (fun acc form ->
      let (formal_name, formal_type) = form in
      sval_decl(formal_type, formal_type, SNoexpr) :: acc
    ) env.vsymtab fformals in
    let nenv = { env with vsymtab = nvsymtab; return_t = frt } in
    let cfbody = check_stmt fbody nenv in
    let sfdecl = sfunc(fname, fformals, frt, cfbody) in
    (sfdecl, nenv)) and

check_stmt_list (sl : stmt list) (ret : bool) (env : scope) =
  let _ = (
    if ret then
      try ignore (List.find (fun s -> match s with
            Return _ -> true
          | _ -> false) sl)
      with Not_found -> raise Failure("This function must return")
    else ()
  ) in let (csl, nenv) = (List.fold_left (fun acc st ->
    let (sl', env') = (check_stmt st (snd acc)) in (sl' :: fst acc, env')
  ) ([], env) sl) in (List.rev csl, nenv)

let check_message_decl (mdecl : message) (env : scope) =
  (try
    let _ = List.find (fun m -> m.m_name = mdecl.m_name) env.messages in
    raise Failure("Message " ^ mdecl.m_name ^ " declared already")
  with Not_found ->
    let smdecl = smessage(mdecl.m_name, mdecl.m_formals) in
    let nenv = {env with messages = smdecl :: env.messages} in
    (smdecl, nenv)) in

let check_actor_decl (adecl : actor) (env : scope) =
  let check_receive (sm : smessage) (patterns : (string, formal list) list) =
    (try
      let _ = List.find (fun p ->
        let formals = List.map (fun (_, t) -> t) snd p in
        (fst p = sm.sm_name) && check_args_t sm.sm_formal formals
      ) patterns in true
    with Not_found -> false) in

  let {aname; aformals; abody; areceive} = adecl in
  (try
    let _ = List.find (fun (a, _, _) -> a.a_name = aname) env.actors in
    raise Failure("Actor " ^ adecl.a_name ^ " declared already")
  with Not_found ->
    let nvsymtab = List.fold_left (fun acc form ->
      let (formal_name, formal_type) = form in
      sval_decl(formal_type, formal_type, SNoexpr) :: acc
    ) env.vsymtab aformals in

    let spatterns = List.map (fun (m, p, _) -> (m, p)) adecl.a_receive in
    let dup_pattern = List.length (List.filter (fun p ->
      try
        let _ = List.find (fun pp -> pp = p) in true
      with Not_found -> false
    ) spatterns) > 0 in
    let m_allowed = List.filter (fun sm ->
      check_receive sm sptterns
    ) env.messages in
    if (List.length m_allowed) <> (List.length adecl.a_receive) then
      raise Failure("Actor " ^ adecl.a_name ^ " attempts to receive " ^
                "undefined messages")
    else
      let curr_scope = {env with in_actor = true} in
      let sabody = check_stmt adecl.a_body curr_scope in

      let nenv = {env with actors = :: env.actors } in
      (smdecl, nenv)) and



