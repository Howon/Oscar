open Ast
open Sast

exception Duplicate_decl
exception Type_mismatch
exception Actor_err
exception Pool_err
exception Invalid_equality
exception Invalid_scope of string

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
  a_messages : smessage list;
} and scope = {
  messages    : smessage list;
  actors      : actor_scope list;
  funcs       : sfunc list;
  env_vtable  : vsymtab;
  env_mvtable : mvsymtab;
  return_t    : types option;
  in_actor    : bool;
}

let build_func (sfn : string) (fl : formal list) (rt : types) (body : sstmt) =
  { sf_name = sfn; sf_formals = fl; sf_return_t = rt; sf_body = body; }

let empty_body = SExpr (SNoexpr, Unit_t)

let upd_env_vtable (vtab : vsymtab) (env : scope) =
  { env with env_vtable = vtab }

let upd_env_mvtable (mvtab : mvsymtab) (env : scope) =
  { env with env_mvtable = mvtab }

let upd_env_messages (m: smessage) (env : scope) =
  { env with messages = m :: env.messages }

let upd_env_funcs (f : sfunc) (env : scope) =
  { env with funcs = f :: env.funcs }

let rec find_value_decl (v_name : string) (vstab : vsymtab) =
  try
    List.find (fun sval -> sval.sv_name = v_name) vstab.svals
  with Not_found ->
    match vstab.vparent with
        Some(vparent) -> find_value_decl v_name vparent
      | _ -> raise Not_found

let rec find_variable_decl (mv_name : string) (mvstab : mvsymtab) =
  try
    List.find (fun mvar -> mvar.smv_name = mv_name) mvstab.smvars
  with Not_found ->
    match mvstab.mvparent with
        Some mvparent -> find_variable_decl mv_name mvparent
      | _ -> raise Not_found

let is_immu (v_name : string) (vstab : vsymtab) =
  try
    let _ = find_value_decl v_name vstab in true
  with
     Not_found -> false

let is_mut (mv_name : string) (mvstab: mvsymtab) =
  try
    let _ = find_variable_decl mv_name mvstab in true
  with
    Not_found -> false

let find_vtype (vstab : vsymtab) (mvstab : mvsymtab) (name : string) =
  try
    let vtype = find_value_decl name vstab in
    try
      let mvtype = find_variable_decl name mvstab in mvtype.smv_type
    with
        Not_found -> vtype.sv_type
      | _ -> raise Duplicate_decl
  with Not_found ->
    try
      let mvtype = find_variable_decl name mvstab in mvtype.smv_type
    with Not_found -> raise Not_found

let find_message (sm_name : string) (env : scope) =
  try
    List.find (fun sm -> sm_name = sm.sm_name) env.messages
  with Not_found ->
    raise (Failure ("Message of type " ^ sm_name ^ " not found"))

let find_actor_scope (sa_name : string) (env : scope) =
  try
    let actor_scope = List.find (fun a_scope ->
      sa_name = a_scope.a_actor.sa_name
    ) env.actors in actor_scope
  with Not_found ->
    raise (Failure ("Actor of type " ^ sa_name ^ " not found"))

let find_actor (sa_name : string) (env : scope) =
  let actor_scope = find_actor_scope sa_name env in
  actor_scope.a_actor

let find_funcs (sf_name : string) (env : scope)  =
  try
    List.filter (fun sfun -> sf_name = sfun.sf_name ) env.funcs
  with
    Not_found -> raise (Failure ("Function of name " ^ sf_name ^ " not found"))

let rec types_equal (t1 : types) (t2 : types) =
  match t1, t2 with
      Int_t, Int_t       -> true
    | Double_t, Double_t -> true
    | Char_t, Char_t     -> true
    | String_t, String_t -> true
    | Bool_t, Bool_t     -> true
    | List_t lt1, List_t lt2 when types_equal lt1 lt2 -> true
    | Set_t st1, Set_t st2 when types_equal st1 st2 -> true
    | Map_t (kt1, vt1), Map_t (kt2, vt2) when
        types_equal kt1 kt2 && types_equal vt1 vt2 -> true
    | _ -> false

let check_binop (te1 : t_expr) (te2 : t_expr)
    (op : bin_op) (env : scope) =
  let (e1, t1) = te1 and (e2, t2) = te2 in
  match op with
      Add ->
        (match t1, t2 with
            Int_t, Int_t  -> (SBinop (e1, op, e2), Int_t)
          | Double_t, Double_t -> (SBinop (e1, op, e2), Double_t)
          | String_t, String_t -> (SBinop (e1, op, e2), String_t)
          | _ -> raise (Failure ("operand type mismatch: " ^
                            (str_types t1) ^ " " ^ (str_types t2))))
    | Sub | Mult | Div | Less | Leq | Greater | Geq ->
        (match t1, t2 with
            Int_t, Int_t  -> (SBinop (e1, op, e2), Int_t)
          | Double_t, Double_t -> (SBinop (e1, op, e2), Double_t)
          | _ -> raise (Failure ("operand type mismatch: " ^
                   (str_types t1) ^ " " ^ (str_types t2))))
    | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
        (match t1, t2 with
            Int_t, Int_t  -> (SBinop (e1, op, e2), Int_t)
          | _ -> raise (Failure ("operand type mismatch: " ^
                   (str_types t1) ^ " " ^ (str_types t2))))
    | Equal | Neq ->
          (if types_equal t1 t2 then
            (SBinop(e1, op, e1), t1)
          else
            match t1, t1 with
              Actor_t _, Actor_t _ ->
                raise (Failure "Actors cannot be compared for equality")
            | Pool_t _, Pool_t _ ->
                raise (Failure "Pools cannot be compared for equality")
            | _ -> raise (Failure ("Cannot compare " ^ str_types t1 ^ " with " ^
                     str_types t2)))
    | And | Or ->
        (match t1, t2 with
            Bool_t, Bool_t -> (SBinop (e1, op, e2), Bool_t)
          | _ -> raise (Failure ("Only boolean expressions are allowed for " ^
                   str_binop op)))
    | Assign ->
        match e1 with
            SId s | SAccess (SId s, _) ->
              (if is_immu s env.env_vtable then
                raise (Failure ("Reassignment to a value " ^ s))
              else
                if (is_mut s env.env_mvtable) then
                  if types_equal t1 t2 then
                    (SBinop(e1, op, e1), t1)
                  else
                    raise (Failure ("Assignment to incompatible type: " ^
                      str_formal (s, t1) ^ " cannot be " ^ "asigned to " ^
                        str_types t2))
               else raise (Failure ("Identifier not found: " ^ s)))
          | SAccess _ -> (SBinop (e1, op, e2), t2)
          | _ -> raise (Failure ("Cannot assign " ^ str_types t1 ^  " as " ^
                   str_types t2))

let check_uop (te : t_expr) (op : u_op) =
  let (e, t) = te in match op with
      Neg -> (match t with
          Int_t    -> (SUop (op, e), Int_t)
        | Double_t -> (SUop (op, e), Double_t)
        | _ -> raise (Failure ("operand type mismatch: " ^ str_uop op ^
                 " on " ^ str_types t)))
    | Not -> (match t with
          Bool_t -> (SUop (op, e), Bool_t)
        | _ -> raise (Failure ("operand type mismatch: " ^ str_uop op ^
                 " on " ^ str_types t)))

let get_list_fst l  =
  List.map (fun (x, _) -> x) l

let get_list_snd l =
  List.map (fun (_, t) -> t) l

let rec check_args_t (params : types list) (args : types list) =
  if not (List.length params = List.length args) then
    false
  else
    try
      List.iter2 (fun t1 t2 -> (match t1, t2 with
          Lambda_t (args1, rt1), Lambda_t (args2, rt2) ->
            let types_match = types_equal rt1 rt2 in
            let args_match = check_args_t args1 args2 in
            if (not types_match || not args_match) then
              raise Type_mismatch
            else ()
        | _ ->
            if (t1 != t2) then
              raise Type_mismatch
            else ()
      )) params args;
      true
    with Type_mismatch -> false

let check_args (params : types list) (args : t_expr list) =
  check_args_t params (get_list_snd args)

let check_message_lit (sm: smessage) (args : t_expr list) =
  let req_params = get_list_snd sm.sm_formals in
  if check_args req_params args then
    let sargs = get_list_fst args in
    (SMessage_Lit (sm.sm_name, sargs), Message_t sm.sm_name)
  else
    raise (Failure ("Message constructed with conflicting parameter types " ^
      sm.sm_name ^ " requires (" ^ str_types_list req_params ^
        ") but constructed with " ^ str_types_list (get_list_snd args)))

let check_actor_lit (sa : sactor) (args : t_expr list) =
  let req_params = get_list_snd sa.sa_formals in
  if check_args req_params args then
    let sargs = get_list_fst args in
      (SActor_Lit (sa.sa_name, sargs), Actor_t sa.sa_name)
  else
    raise (Failure ("Actor constructed with conflicting parameter types " ^
      sa.sa_name ^ " requires (" ^ str_types_list req_params ^
        ") but constructed with " ^ str_types_list (get_list_snd args)))

let check_func_call (sf : sfunc) (args : t_expr list) =
  let req_params = get_list_snd sf.sf_formals in
    check_args req_params args

let rec check_expr (e : expr) (env : scope) =
  let check_exprl (el : expr list) =
    List.map (fun ex -> check_expr ex env) el in

  match e with
      Int_Lit i           -> (SInt_Lit i, Int_t)
    | Double_Lit d        -> (SDouble_Lit d, Double_t)
    | Char_Lit c          -> (SChar_Lit c, Char_t)
    | String_Lit s        -> (SString_Lit s, String_t)
    | Bool_Lit b          -> (SBool_Lit b, Bool_t)
    | Unit_Lit u          -> (SUnit_Lit u, Unit_t)
    | Id id ->
        (try
          let v_t = find_vtype env.env_vtable env.env_mvtable id in
            (SId id, v_t)
        with
            Not_found ->
              raise (Failure ("Undeclared identifier " ^ id))
          | Duplicate_decl ->
              raise (Failure (id ^ " declared as both mutable and immutable")))
    | Access(e1, e2) ->
        let (se1, t1) = check_expr e1 env and
          (se2, t2) = check_expr e2 env in
          (match t1, t2 with
              List_t lt, Int_t               -> (SAccess(se1, se2), lt)
            | Set_t st, _ when st = t2       -> (SAccess(se1, se2), st)
            | Map_t (kt, vt), _ when kt = t2  -> (SAccess(se1, se2), vt)
            | _ -> raise (Failure ("Invalid access types " ^ str_types t1 ^
                     " cannot be accessed by " ^ str_types t2)))
    | Lambda ld ->
        let {l_formals; l_return_t; l_body} = ld in
          let slambda = SLambda {
            sl_formals  = l_formals;
            sl_return_t = l_return_t;
            sl_body     = fst (check_stmt l_body env)
          } in (slambda, Lambda_t (get_list_snd l_formals, l_return_t))
    | List_Lit (lt, ex) ->
        let te_list = check_exprl ex in
          let sexpr_list = get_list_fst te_list in
            (SList_Lit (lt, sexpr_list), List_t lt)
    | Set_Lit (st, ex) ->
        let te_list = check_exprl ex in
          let sexpr_list = get_list_fst te_list in
            (SSet_Lit (st, sexpr_list), Set_t st)
    | Map_Lit (kt, vt, kvx) ->
        let tkv_list= List.map (fun (k, v) ->
          ((check_expr k env), (check_expr v env))
        ) kvx in
          let skv_list = List.map (fun ((k, _), (t, _)) -> (k, t)) tkv_list in
            (SMap_Lit (kt, vt, skv_list), Map_t (kt, vt))
    | Actor_Lit (at, ex) ->
        let sactor = find_actor at env in
        let te_list = check_exprl ex in
        check_actor_lit sactor te_list
    | Pool_Lit (at, ex, num) ->
        let (sa_num, num_act) = check_expr num env in
        (match sa_num with
            SInt_Lit x ->
              if x < 1 then
                raise (Failure "Number of actors in a pool must be at least 1")
              else
                let sactor = find_actor at env in
                  let te_list = check_exprl ex in
                    (match check_actor_lit sactor te_list with
                        SActor_Lit (sa_id, sa_args), Actor_t a_t ->
                          (SPool_Lit (sa_id, sa_args, sa_num), Pool_t a_t)
                      | _ -> raise (Failure ("Cannot create a pool with " ^
                               "undeclared actor " ^ at )))
          | _ -> raise (Failure "Number of actors must be an integer"))
    | Message_Lit (m, ex) ->
        let smessage = find_message m env in
          let te_list = check_exprl ex in
            check_message_lit smessage te_list
    | Binop (e1, op, e2) ->
        let checked_e1 = check_expr e1 env and
          checked_e2 = check_expr e2 env in
            check_binop checked_e1 checked_e2 op env
    | Uop (op, e) ->
        let checked_e = check_expr e env in check_uop checked_e op
    | FuncCall (f, args) ->
        let sfs = find_funcs f env in
          let tex_args = check_exprl args in
            (try
              let sf = List.find (fun f ->
                check_func_call f tex_args
              ) sfs in
                let sargs = get_list_fst tex_args in
                  (SFuncCall (sf.sf_name, sargs), sf.sf_return_t)
            with Not_found ->
              raise (Failure ("Function " ^ f ^ " with signature (" ^
                str_types_list (get_list_snd tex_args) ^ ") => " ^ str_types (
                    match env.return_t with
                      Some rt -> rt
                    | None    -> Unit_t
                  ) ^ " not found")))
    | Noexpr -> (SNoexpr, Unit_t)

and check_stmt (s : stmt) (env : scope) =
  match s with
      Block sl ->
        let nvtable = { vparent = Some env.env_vtable; svals = []; } in
        let nmv_table = { mvparent = Some env.env_mvtable; smvars  = []; } in
        let nenv = upd_env_vtable nvtable (upd_env_mvtable nmv_table env) in
        let must_return = (match nenv.return_t with
            Some rt when rt != Unit_t -> true
          | _       -> false
        ) in let (checked_stmts, _) = check_stmt_list sl must_return nenv in
        (SBlock checked_stmts, env)
    | Expr e -> (SExpr (check_expr e env), env)
    | Return e ->
        let (sexpr, t) = check_expr e env in
        (match env.return_t with
            Some rt ->
              if (t = rt) then
                (SReturn (sexpr), env)
              else
                raise (Failure ("Return type mismatch: expected " ^
                  str_types rt ^ " but returns " ^ str_types t))
          | _ -> raise (Failure "This function does not return when required"))
    | Vdecl vdecl -> check_vdecl vdecl env
    | Mutdecl mvdecl -> check_mvdecl mvdecl env
    | Fdecl fdecl ->
        check_func_decl fdecl env
    | If(cond, isl, esl) ->
        let (se, t) = check_expr cond env in
        (match t with
            Bool_t ->
              let (check_if, _) = check_stmt isl env in
              let (check_esl, _) = check_stmt esl env in
              (SIf(se, check_if, check_esl), env)
          | _ -> raise (Failure "Condition must be a boolean"))
    | Actor_send (e1, e2)->
        let (se1, st1) = check_expr e1 env in
        let (se2, st2) = check_expr e2 env in
        (match st1, st2 with
            Message_t m_name, Actor_t a_name ->
              let sm = find_message m_name env in
              let sm_allowed = (find_actor_scope a_name env).a_messages in
              (try
                let _ = List.find (fun allowed_m ->
                  allowed_m.sm_name = sm.sm_name
                ) sm_allowed in
                (SActor_send (se1, se2), env)
              with Not_found ->
                raise (Failure ("No matching pattern to receive message " ^
                  m_name ^ " in actor " ^ a_name)))
          | _ -> raise (Failure ("Invalid send operation between " ^
                   str_types st1 ^ " to " ^
                     str_types st2)))
    | Pool_send (e1, e2) ->
        let (se1, st1) = check_expr e1 env in
        let (se2, st2) = check_expr e2 env in
        (match st1, st2 with
            Message_t m_name, Pool_t a_name ->
              let sm = find_message m_name env in
              let sm_allowed = (find_actor_scope a_name env).a_messages in
              (try
                let _ = List.find (fun allowed_m ->
                  allowed_m.sm_name = sm.sm_name
                ) sm_allowed in
                (SActor_send (se1, se2), env)
              with Not_found ->
                raise (Failure ("No matching pattern to receive message " ^
                  m_name ^ " in actor " ^ a_name)))
          | _ -> raise (Failure ("Invalid send operation between " ^
                   str_types st1 ^ " to " ^ str_types st2)))

and check_vdecl (vdecl : val_decl) (env : scope) =
  let {v_name; v_type; v_init} = vdecl in
  (match v_type with
      Lambda_t (_, _) -> raise (Failure "Cannot declare lambda types")
    | _ ->
      let (se, t) = check_expr v_init env in
      match se with
          SNoexpr -> raise (Failure ("Must initialize value " ^ v_name))
        | _ ->
            if t = v_type then
              (try
                let _ = (List.find (fun sval ->
                  sval.sv_name = v_name
                ) env.env_vtable.svals) in
                raise (Failure ("Value " ^ v_name ^ " declared already"))
              with Not_found ->
                let svdecl = {
                  sv_name = v_name;
                  sv_type = v_type;
                  sv_init = se
                } in
                let nvsymbt = {
                  env.env_vtable with svals = svdecl :: env.env_vtable.svals;
                } in (SVdecl svdecl, upd_env_vtable nvsymbt env))
            else
              raise (Failure ("Value initialization type mismatch: " ^
                v_name ^ " is " ^ (str_types v_type) ^ " but initialized as " ^
                  (str_types t))))

and check_mvdecl (mvdecl : mvar_decl) (env : scope) =
  if env.in_actor then
    let {mv_name; mv_type; mv_init} = mvdecl in
    (match mv_type with
        Lambda_t (_, _) -> raise (Failure "Cannot declare lambda types")
      | _ ->
        let (se, t) = check_expr mv_init env in
        let se_t = (match se with
            SNoexpr -> mv_type
          | _ -> t
        ) in
        if se_t = mv_type then
          (try
            let _ = (List.find (fun smvar ->
              smvar.smv_name = mv_name
            ) env.env_mvtable.smvars) in
            raise (Failure ("Mutable var " ^ mv_name ^ " declared already"))
          with Not_found ->
            let smvdecl = {
              smv_name = mv_name;
              smv_type = mv_type;
              smv_init = se;
            } in
            let nmvsymbt = {
              env.env_mvtable with smvars = smvdecl :: env.env_mvtable.smvars;
            } in (SMutdecl smvdecl, upd_env_mvtable nmvsymbt env))
        else
          raise (Failure ("Variable initialization type mismatch: " ^
            mv_name ^ " is " ^ (str_types mv_type) ^
              " but initialized as " ^ (str_types t))))
  else
    raise (Failure ("Mutables types are only allowed in actors"))

and check_func_decl (fdecl : func) (env : scope) =
  let rec gen_temp_name (len : int) =
    match len with
        1 -> [ Char.escaped (Char.chr len) ]
      | 26 -> raise (Failure "Too many arguments for this lambda")
      | _ -> Char.escaped (Char.chr len) :: gen_temp_name (len - 1) in

  let gen_lambda_formals (l_formals : types list) =
    let temp_names = gen_temp_name (List.length l_formals) in
      List.fold_left2 (fun acc n f -> (n, f) :: acc) [] temp_names l_formals in

  let { f_name; f_formals; f_return_t; f_body } = fdecl in
  (try
    let _ = List.find (fun sf ->
      sf.sf_name = f_name &&
        check_args_t (get_list_snd sf.sf_formals) (get_list_snd f_formals)
    ) env.funcs in
    raise (Failure ("Function " ^ f_name ^ " declared already"))
  with Not_found ->
    let (nvals, nfuncs) = List.fold_left (fun acc form ->
      let (formal_name, formal_type) = form in
      match formal_type with
          Lambda_t (lfl, lrt) ->
            let n_funcs = {
              sf_name     = formal_name;
              sf_return_t = lrt;
              sf_formals  = gen_lambda_formals lfl;
              sf_body     = empty_body
            } :: snd acc in
            (fst acc, n_funcs)
        | _ ->
            let n_vals = {
              sv_name = formal_name;
              sv_type = formal_type;
              sv_init = SNoexpr
            } :: fst acc in
            (n_vals, snd acc)
    ) (env.env_vtable.svals, env.funcs) f_formals in
    let nv_table = { env.env_vtable with svals = nvals } in
    let forward_decl = build_func f_name f_formals f_return_t empty_body in
    let fenv = { env with
      funcs = forward_decl :: nfuncs;
      env_vtable = nv_table;
      return_t = Some f_return_t
    } in
    let (cfbody, _) = check_stmt f_body fenv in
    let sfdecl = {
      sf_name     = f_name;
      sf_formals  = f_formals;
      sf_return_t = f_return_t;
      sf_body     = cfbody
    } in let nenv = upd_env_funcs sfdecl env in
    (SFdecl sfdecl, nenv))

and check_stmt_list (sl : stmt list) (ret : bool) (env : scope) =
  let _ = (
    if ret then
      try ignore (List.find (fun s -> match s with
            Return _ -> true
          | _ -> false) sl)
      with Not_found -> raise (Failure "This function must return")
    else ()
  ) in let (csl, nenv) = (List.fold_left (fun acc st ->
    let (sl', env') = (check_stmt st (snd acc)) in (sl' :: fst acc, env')
  ) ([], env) sl) in (List.rev csl, nenv)

let check_message_decl (mdecl : message) (env : scope) =
  (try
    let _ = List.find (fun sm -> sm.sm_name = mdecl.m_name) env.messages in
    raise (Failure ("Message " ^ mdecl.m_name ^ " declared already"))
  with Not_found ->
    let smdecl = {
      sm_name = mdecl.m_name;
      sm_formals = mdecl.m_formals
    } in (smdecl, upd_env_messages smdecl env))

let check_actor_decl (adecl : actor) (env : scope) =
  let check_receive (sm : smessage) (patterns : pattern list) =
    let p_formals = List.map (fun p -> (p.p_mid, p.p_mformals)) patterns in
    (try
      let _ = List.find (fun p ->
        let p_formal_ts = get_list_snd (snd p) in
        let m_formal_ts = get_list_snd sm.sm_formals in
        (fst p = sm.sm_name) && check_args_t m_formal_ts p_formal_ts
      ) p_formals in true
    with Not_found -> false) in

  let {a_name; a_formals; a_body; a_receive} = adecl in
  (try
    let _ = List.find (fun ascope ->
      ascope.a_actor.sa_name = a_name
    ) env.actors in
    raise (Failure ("Actor " ^ adecl.a_name ^ " declared already"))
  with Not_found ->
    let rec check_dup l = (match l with
          [] -> false
        | (h :: t) ->
            let x = (List.filter (fun x -> x = h) t) in
            if (x == []) then
              check_dup t
            else
              true
      ) in if check_dup (List.map(fun p -> p.p_mid) a_receive) then
        raise (Failure ("Duplicate pattern matching in receive in actor " ^
          a_name))
    else
      let m_allowed = List.filter (fun sm ->
        check_receive sm a_receive
      ) env.messages in
      if (List.length m_allowed) <> (List.length adecl.a_receive) then
        raise (Failure ("Actor " ^ adecl.a_name ^ " attempts to receive " ^
          "an undefined message"))
      else
        let nsvals = List.fold_left (fun acc form ->
          let (formal_name, formal_type) = form in
          {
            sv_name = formal_name;
            sv_type = formal_type;
            sv_init = SNoexpr
          } :: acc
        ) env.env_vtable.svals a_formals in
        let nv_table = { env.env_vtable with svals = nsvals } in
        let curr_scope = { env with env_vtable = nv_table; in_actor = true } in
        let (checked_body, _) = check_stmt adecl.a_body curr_scope in
        let sareceive = List.map (fun p ->
          let (checked_pbody, _) = check_stmt p.p_body curr_scope in
          {
            sp_smid = p.p_mid;
            sp_smformals = p.p_mformals;
            sp_body = checked_pbody
          }
        ) a_receive in
        let new_sactor = {
          sa_name    = a_name;
          sa_formals = a_formals;
          sa_body    = checked_body;
          sa_receive = sareceive
        } in
        let actor_env = upd_env_vtable nv_table env in
        let new_actor_scope = {
          a_actor = new_sactor;
          a_scope = actor_env;
          a_messages = m_allowed
        } in
        let nenv = { env with actors = new_actor_scope :: env.actors } in
        (new_actor_scope, nenv))

let check_program (p : program) =
  let (messages, actors, functions) = p in
  let empty_vsymtab = { vparent = None; svals = [] } in
  let empty_mvsymtab = { mvparent = None; smvars =  [] } in
  let stdlib_funcs =
  [
    build_func "println" [("", String_t)] Unit_t empty_body;
    build_func "println" [("", Int_t)] Unit_t empty_body;
    build_func "println" [("", Double_t)] Unit_t empty_body;
    build_func "println" [("", Char_t)] Unit_t empty_body;
    build_func "println" [("", Bool_t)] Unit_t empty_body;
  ] in
  let seed_env = {
    messages = [];
    actors = [];
    funcs = stdlib_funcs;
    env_vtable = empty_vsymtab;
    env_mvtable = empty_mvsymtab;
    return_t = None;
    in_actor = false;
  } in
  let (smessages, m_env) = List.fold_left (fun acc m ->
    let (smessage, nenv) = check_message_decl m (snd acc) in
    (smessage :: fst acc, nenv)
  ) ([], seed_env) messages in
  let (sactors, a_env) = List.fold_left (fun acc a ->
    let (a_scope, nenv) = check_actor_decl a (snd acc) in
    (a_scope.a_actor :: fst acc, nenv)
  ) ([], m_env) actors in
  let (sfunctions, _) = List.fold_left (fun acc f ->
    let (sfunc, nenv) = check_func_decl f (snd acc) in
    match sfunc with
        SFdecl sf -> (sf :: fst acc, nenv)
      | _ -> raise (Failure ("Not a valid function: " ^ f.f_name))
  ) ([], a_env) functions in
  try
    let _ = List.find (fun sf -> sf.sf_name = "main") sfunctions in
    (smessages, sactors, sfunctions)
  with Not_found -> raise (Failure "No main function in this program")



