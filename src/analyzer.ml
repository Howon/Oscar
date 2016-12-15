open Ast
open Sast

exception Duplicate_decl
exception Type_mismatch
exception Builtin_arg_num_err of string * int * int
exception Builtin_arg_type_err of string * (t_expr list)

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
  actor_init    : bool;
}

let get_list_fst l  =
  List.map (fun (x, _) -> x) l

let get_list_snd l =
  List.map (fun (_, t) -> t) l

let build_func (sfn : string) (fl : formal list) (rt : types) (body : sstmt) =
  { sf_name = sfn; sf_formals = fl; sf_return_t = rt; sf_body = body; }

let empty_fbody = SExpr (SNoexpr, Unit_t)

let upd_vtable_vals (nsval : sval_decl) (v_table : vsymtab) =
  { v_table with svals = nsval :: v_table.svals }

let upd_mvtable_vals (nmsvar : smvar_decl) (mv_table : mvsymtab) =
  { mv_table with smvars = nmsvar :: mv_table.smvars }

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

let find_vtype (name : string) (env : scope) =
  let vstab = env.env_vtable and mvstab = env.env_mvtable in
    try
      let vtype = find_value_decl name vstab in
      try
        let mvtype = find_variable_decl name mvstab in mvtype.smv_type
      with
          Not_found -> vtype.sv_type
        | _ -> raise Duplicate_decl
    with Not_found ->
      let mvtype = find_variable_decl name mvstab in mvtype.smv_type

let is_type_apm (t : types) =
  match t with
      Actor_t _ | Pool_t _ | Message_t _ -> true
    | _ -> false

let validate_arg_types (args : types list) =
  try
    List.fold_left (fun acc arg_t ->
       acc && not (is_type_apm arg_t)
    ) true args
  with Not_found -> true

let rec types_equal (t1 : types) (t2 : types) =
  match t1, t2 with
      Int_t, Int_t       -> true
    | Double_t, Double_t -> true
    | Char_t, Char_t     -> true
    | String_t, String_t -> true
    | Bool_t, Bool_t     -> true
    | Unit_t, Unit_t     -> true
    | Lambda_t (fl1, rt1), Lambda_t (fl2, rt2) ->
        check_args_t fl1 fl2 && types_equal rt1 rt2
    | List_t lt1, List_t lt2 when types_equal lt1 lt2 -> true
    | Set_t st1, Set_t st2 when types_equal st1 st2 -> true
    | Map_t (kt1, vt1), Map_t (kt2, vt2) when
        types_equal kt1 kt2 && types_equal vt1 vt2 -> true
    | _ -> false

and check_args_t (params : types list) (args : types list) =
  if not (List.length params = List.length args) then
    false
  else
    List.fold_left2 (fun acc p1 p2 -> acc && types_equal p1 p2) true params args

and types_equal' (t1 : types) (t2 : types) =
  types_equal t1 t2 || (
    match t1, t2 with
        Message_t mt1, Message_t mt2 when mt1 = mt2 -> true
      | Actor_t at1, Actor_t at2 when at1 = at2 -> true
      | Pool_t pt1, Pool_t pt2 when pt1 = pt2 -> true
      | _ -> false
  )

let check_args (params : types list) (args : t_expr list) =
  check_args_t params (get_list_snd args)

let find_func (f_name : string) (param : types list) (env : scope)  =
  List.find (fun sf ->
    sf.sf_name = f_name &&
      check_args_t (get_list_snd sf.sf_formals) param
  ) env.funcs

let func_exists (f_name : string) (param : types list) (env : scope) =
  try
    let _ = find_func f_name param env in true
  with
    _ -> false

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

let name_taken (name : string) (env : scope) =
  (List.fold_left (fun acc sf ->
    (sf.sf_name = name) || acc
  ) false env.funcs) || (
    try
      let _ = find_vtype name env in true
    with Not_found -> false
  )

let find_actor (sa_name : string) (env : scope) =
  let actor_scope = find_actor_scope sa_name env in
  actor_scope.a_actor

let check_message_lit (sm: smessage) (args : t_expr list) =
  let req_params = get_list_snd sm.sm_formals in
  if check_args req_params args then
    (SMessage_Lit (sm.sm_name, args), Message_t sm.sm_name)
  else
    raise (Failure ("Message constructed with conflicting parameter types " ^
      sm.sm_name ^ " requires (" ^ str_types_list req_params ^
        ") but constructed with " ^ str_types_list (get_list_snd args)))

let check_actor_lit (sa : sactor) (args : t_expr list) =
  let req_params = get_list_snd sa.sa_formals in
  if check_args req_params args then
    (SActor_Lit (sa.sa_name, args), Actor_t sa.sa_name)
  else
    raise (Failure ("Actor constructed with conflicting parameter types " ^
      sa.sa_name ^ " requires (" ^ str_types_list req_params ^
        ") but constructed with " ^ str_types_list (get_list_snd args)))

let check_builtin (f : string) (tel : t_expr list) (env : scope) =
  let args_len = List.length tel in
    match f with
        "Println" ->
          (match tel with
              [(_, tail_t)] ->
                (match tail_t with
                    Actor_t _ | Pool_t _ | Message_t _ ->
                      raise (Builtin_arg_type_err (f, tel))
                  | _ -> (SFuncCall(f, tel), Unit_t))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "AsInt" ->
          (match tel with
            [(_, tail_t)] ->
                (match tail_t with
                    Double_t -> (SFuncCall(f, tel), Int_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "AsDouble" ->
          (match tel with
            [(_, tail_t)] ->
                (match tail_t with
                    Int_t -> (SFuncCall(f, tel), Double_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "AsString" ->
          (match tel with
            [(_, tail_t)] ->
                (match tail_t with
                    Int_t | Double_t | Char_t | Bool_t ->
                      (SFuncCall(f, tel), String_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "Reverse" ->
          (match tel with
              [(_, tail_t)] ->
                (match tail_t with
                    List_t _ -> (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "Size" ->
          (match tel with
              [(_, tail_t)] ->
                (match tail_t with
                    List_t _ | Set_t _ | Map_t (_, _) ->
                      (SFuncCall(f, tel), Int_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 1, args_len)))
      | "Contains" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match tail_t with
                    List_t ct | Set_t ct | Map_t (ct, _) when
                      types_equal head_t ct -> (SFuncCall(f, tel), Bool_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Prepend" | "Append" | " PopFront" | "PopBack"->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match tail_t with
                    List_t ct when
                      types_equal head_t ct -> (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "MergeFront" | "MergeBack"->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                    List_t lt1, List_t lt2 when
                      types_equal lt1 lt2 -> (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Union" | "Diff" | "Intersection" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                    Set_t st1, Set_t st2 when
                      types_equal st1 st2 -> (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Put" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match tail_t with
                    Set_t ct when
                      types_equal head_t ct -> (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | (_, head_t) :: [(_, v_t) ; (_, tail_t)] ->
                (match tail_t with
                    Map_t (kt, vt) when
                      types_equal head_t kt && types_equal v_t vt ->
                        (SFuncCall(f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "ForEach" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                  Lambda_t ([ft], rt), (List_t ct | Set_t ct) when
                    types_equal ft ct && types_equal rt Unit_t ->
                      (SFuncCall(f, tel), Unit_t)
                | Lambda_t (fkt :: [fvt], rt), Map_t (kt, vt) when
                    types_equal fkt kt && types_equal fvt kt &&
                      types_equal rt Unit_t -> (SFuncCall (f, tel), Unit_t)
                | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Map" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                    Lambda_t ([ft], rt), List_t lt when types_equal ft lt ->
                      (SFuncCall (f, tel), List_t rt)
                  | Lambda_t ([ft], rt), Set_t st when types_equal ft st ->
                      (SFuncCall (f, tel), Set_t rt)
                  | Lambda_t (fkt :: [fvt], rt), Map_t (kt, vt) when
                      types_equal fkt kt && types_equal fvt kt ->
                        (SFuncCall (f, tel), Map_t (kt, rt))
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Filter" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                    Lambda_t ([ft], rt), (List_t ct | Set_t ct) when
                      types_equal ft ct && types_equal rt Bool_t ->
                        (SFuncCall (f, tel), tail_t)
                  | Lambda_t (fkt :: [fvt], rt), Map_t (kt, vt) when
                      types_equal fkt kt && types_equal fvt kt &&
                        types_equal rt Bool_t -> (SFuncCall (f, tel), tail_t)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "FoldLeft" ->
          (match tel with
              (_, head_t) :: [(_, acc) ; (_, tail_t)] ->
                (match head_t, tail_t with
                    Lambda_t (ft :: [acct], rt), List_t ct when
                      types_equal ft ct && types_equal acc acct &&
                        types_equal acc rt -> (SFuncCall(f, tel), rt)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | "Reduce" ->
          (match tel with
              (_, head_t) :: [(_, tail_t)] ->
                (match head_t, tail_t with
                    Lambda_t (ft :: [acc], rt), List_t ct  when
                      types_equal ft ct && types_equal acc rt ->
                        (SFuncCall(f, tel), rt)
                  | _ -> raise (Builtin_arg_type_err (f, tel)))
            | _ -> raise (Builtin_arg_num_err (f, 2, args_len)))
      | _ -> raise Not_found


let check_binop (te1 : t_expr) (te2 : t_expr)
    (op : bin_op) (env : scope) =
  let (e1, t1) = te1 and (e2, t2) = te2 in
    match op with
        Add ->
          (match t1, t2 with
              Int_t, Int_t       -> (SBinop (te1, op, te2), Int_t)
            | Double_t, Double_t -> (SBinop (te1, op, te2), Double_t)
            | String_t, String_t -> (SBinop (te1, op, te2), String_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                     (str_types t1) ^ " " ^ str_binop op ^ " " ^
                       (str_types t2))))
      | Sub | Mult | Div ->
          (match t1, t2 with
              Int_t, Int_t       -> (SBinop (te1, op, te2), Int_t)
            | Double_t, Double_t -> (SBinop (te1, op, te2), Double_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                     (str_types t1) ^ " " ^ str_binop op ^ " " ^
                       (str_types t2))))
      | Less | Leq | Greater | Geq ->
          (match t1, t2 with
              Int_t, Int_t       -> (SBinop (te1, op, te2), Bool_t)
            | Double_t, Double_t -> (SBinop (te1, op, te2), Bool_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                     (str_types t1) ^ " " ^ str_binop op ^ " " ^
                       (str_types t2))))
      | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop (te1, op, te2), Int_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                     (str_types t1) ^ " " ^ str_binop op ^ " " ^
                       (str_types t2))))
      | Equal | Neq ->
            (if types_equal t1 t2 then
              (SBinop(te1, op, te2), Bool_t)
            else
              match t1, t2 with
                  Actor_t _, Actor_t _ ->
                    raise (Failure "Actors cannot be compared for equality")
                | Pool_t _, Pool_t _ ->
                    raise (Failure "Pools cannot be compared for equality")
                | Message_t _, Message_t _ ->
                    raise (Failure "Messages cannot be compared for equality")
                | _ -> raise (Failure ("Cannot compare " ^ str_types t1 ^
                         " with " ^ str_types t2)))
      | And | Or ->
          (match t1, t2 with
              Bool_t, Bool_t -> (SBinop (te1, op, te2), Bool_t)
            | _ -> raise (Failure ("Only boolean expressions are allowed for " ^
                     str_binop op)))
      | Assign ->
          match e1 with
              SId s | SAccess ((SId s, _), _) ->
                (if is_immu s env.env_vtable then
                  raise (Failure ("Reassignment to a value " ^ s))
                else
                  if (is_mut s env.env_mvtable) then
                    if types_equal t1 t2 then
                      (SBinop(te1, op, te2), t1)
                    else
                      match t1, t2 with
                          Actor_t _, Actor_t _ ->
                            raise (Failure "Actors cannot be reassigned")
                        | Pool_t _, Pool_t _ ->
                            raise (Failure "Pools cannot be reassigned")
                        | Message_t _, Message_t _ ->
                            raise (Failure "Messages cannot be reassigned")
                        | _ -> raise (Failure ("Assignment to incompatible " ^
                                 "types " ^ str_types t1 ^ " cannot be " ^
                                   "asigned to " ^ str_types t2))
                 else
                   raise (Failure ("Identifier not found: " ^ s)))
            | SAccess _ -> (SBinop (te1, op, te2), t2)
            | _ -> raise (Failure ("Cannot assign " ^ str_types t1 ^  " as " ^
                     str_types t2))

let check_uop (te : t_expr) (op : u_op) =
  let (e, t) = te in match op with
      Neg ->
        (match t with
            Int_t    -> (SUop (op, te), Int_t)
          | Double_t -> (SUop (op, te), Double_t)
          | _ -> raise (Failure ("operand type mismatch: " ^ str_uop op ^
                   " on " ^ str_types t)))
    | Not ->
        (match t with
            Bool_t -> (SUop (op, te), Bool_t)
          | _ -> raise (Failure ("operand type mismatch: " ^ str_uop op ^
                   " on " ^ str_types t)))

let spread_arg (args : formal list) (vals : sval_decl list)
    (funcs : sfunc list) (env : scope) =
  let rec gen_temp_name (len : int) =
    match len with
        1  -> [ Char.escaped (Char.chr len) ]
      | 26 -> raise (Failure "Too many arguments for this lambda")
      | _  -> Char.escaped (Char.chr len) :: gen_temp_name (len - 1) in

  let gen_lambda_formals (l_formals : types list) =
    let temp_names = gen_temp_name (List.length l_formals) in
    List.fold_left2 (fun acc n f -> (n, f) :: acc) [] temp_names l_formals in

  List.fold_left (fun acc form ->
    let (formal_name, formal_type) = form in
      match formal_type with
          Lambda_t (lfl, lrt) ->
            let n_funcs = {
              sf_name     = formal_name;
              sf_return_t = lrt;
              sf_formals  = gen_lambda_formals lfl;
              sf_body     = empty_fbody
            } :: snd acc in
            (fst acc, n_funcs)
        | _ ->
            let n_vals = {
              sv_name = formal_name;
              sv_type = formal_type;
              sv_init = (SNoexpr, formal_type)
            } :: fst acc in
            (n_vals, snd acc)
  ) (vals, funcs) args

let rec check_expr (e : expr) (env : scope) =
  let type_consistent (typ : types) (tel : types list) (cont : string) =
    try
      (let _ = List.find (fun t ->
        not (types_equal typ t)
      ) tel in
      raise (Failure (cont ^ " of type " ^ str_types typ ^ " cannot be " ^
        "initialized with parameters of type[s]" ^ str_types_list tel)))
    with Not_found -> () in

  match e with
      Int_Lit i           -> (SInt_Lit i, Int_t)
    | Double_Lit d        -> (SDouble_Lit d, Double_t)
    | Char_Lit c          -> (SChar_Lit c, Char_t)
    | String_Lit s        -> (SString_Lit s, String_t)
    | Bool_Lit b          -> (SBool_Lit b, Bool_t)
    | Unit_Lit u          -> (SUnit_Lit u, Unit_t)
    | Id id ->
        (try
          let v_t = find_vtype id env in
            (SId id, v_t)
        with
            Not_found ->
              raise (Failure ("Undeclared identifier " ^ id))
          | Duplicate_decl ->
              raise (Failure (id ^ " declared as both mutable and immutable")))
    | Access(e1, e2) ->
        let texp1 = check_expr e1 env and texp2 = check_expr e2 env in
        let (se1, t1) = texp1 and (se2, t2) = texp2 in
          (match t1, t2 with
              List_t lt, Int_t -> (SAccess(texp1, texp2), lt)
            | Set_t st, _ when types_equal st t2 -> (SAccess(texp1, texp2), st)
            | Map_t (kt, vt), _ when types_equal kt t2 ->
                                  (SAccess(texp1, texp2), vt)
            | _ -> raise (Failure ("Invalid access types " ^ str_types t1 ^
                     " cannot be accessed by " ^ str_types t2)))
    | Lambda ld ->
        let {l_formals; l_return_t; l_body} = ld in
        let (nvals, nfuncs) = spread_arg l_formals [] env.funcs env in
        let nv_table = { vparent = Some env.env_vtable; svals = nvals } in
        let lenv = { env with
          funcs = nfuncs;
          env_vtable = nv_table;
          return_t = Some l_return_t
        } in
        let (cfbody, _) = check_stmt l_body lenv in
        let slambda = SLambda {
          sl_formals  = l_formals;
          sl_return_t = l_return_t;
          sl_body     = cfbody
        } in (slambda, Lambda_t (get_list_snd l_formals, l_return_t))
    | List_Lit (lt, ex) ->
        let te_list = check_expr_list ex env in
        let te_list_types = get_list_snd te_list in
        let _ = type_consistent lt te_list_types "List" in
          (SList_Lit (lt, te_list), List_t lt)
    | Set_Lit (st, ex) ->
        let te_list = check_expr_list ex env in
        let te_list_types = get_list_snd te_list in
        let _ = type_consistent st te_list_types "Set" in
          (SSet_Lit (st, te_list), Set_t st)
    | Map_Lit (kt, vt, kvx) ->
        let tkv_list = List.map (fun (k, v) ->
          ((check_expr k env), (check_expr v env))
        ) kvx in
          (SMap_Lit (kt, vt, tkv_list), Map_t (kt, vt))
    | Actor_Lit (at, ex) ->
        let sactor = find_actor at env in
        let te_list = check_expr_list ex env in
          check_actor_lit sactor te_list
    | Pool_Lit (at, ex, num) ->
        let (sa_num, sa_t) = check_expr num env in
          (match sa_t with
              Int_t ->
                let sactor = find_actor at env in
                let te_list = check_expr_list ex env in
                  (match check_actor_lit sactor te_list with
                      SActor_Lit (sa_id, sa_args), Actor_t a_t ->
                        (SPool_Lit (sa_id, te_list, (sa_num, Int_t)),
                          Pool_t a_t)
                    | _ -> raise (Failure ("Cannot create a pool with " ^
                             "undeclared actor type " ^ at )))
            | _ -> raise (Failure "Number of actors must be an integer"))
    | Message_Lit (m, ex) ->
        let smessage = find_message m env in
        let te_list = check_expr_list ex env in
          check_message_lit smessage te_list
    | Binop (e1, op, e2) ->
        let checked_e1 = check_expr e1 env and checked_e2 = check_expr e2 env in
          check_binop checked_e1 checked_e2 op env
    | Uop (op, e) ->
        let checked_e = check_expr e env in check_uop checked_e op
    | FuncCall (f, args) ->
        let checked_args = check_expr_list args env in
        let arg_types = get_list_snd checked_args in
        (if validate_arg_types arg_types then
          try
            check_builtin f checked_args env
          with
              Builtin_arg_num_err (b, e, num) ->
                raise (Failure ("Builtin function " ^ b ^ " called with too " ^
                  "many args: Expected " ^ string_of_int e ^ " but got " ^
                    string_of_int num))
            | Builtin_arg_type_err (b, _) ->
                raise (Failure ("Builtin function " ^ b ^ " cannot be called " ^
                  "with arguments of type " ^ str_types_list arg_types))
            | Not_found ->
                (try
                  let sf = find_func f arg_types env in
                    if check_args (get_list_snd sf.sf_formals) checked_args then
                      (SFuncCall(f, checked_args), sf.sf_return_t)
                    else
                      raise (Failure ("Function " ^ f ^ " has signature (" ^
                        str_types_list (get_list_snd sf.sf_formals) ^ ") => " ^
                          str_types (match env.return_t with
                              Some rt -> rt
                            | None    -> Unit_t
                          ) ^ " but was called with args (" ^
                             str_types_list arg_types ^ ")"))
                with Not_found ->
                      raise (Failure ("Function " ^ f ^ " not found")))
        else
          raise (Failure ("Actors, pools and messages cannot be passed into " ^
            "functions as arguments")))
     | Noexpr ->
         (SNoexpr, Unit_t)

and check_expr_list (el : expr list) (env : scope) =
    (List.map (fun ex -> check_expr ex env) el)

and check_stmt (s : stmt) (env : scope) =
  match s with
      Block sl ->
        let nvtable = { vparent = Some env.env_vtable; svals = []; } in
        let nmv_table = { mvparent = Some env.env_mvtable; smvars  = []; } in
        let nenv = upd_env_vtable nvtable (upd_env_mvtable nmv_table env) in
        let must_return =
          (match nenv.return_t with
              Some rt when rt != Unit_t -> true
            | _       -> false
          ) in
        let (checked_stmts, cnenv) = check_stmt_list sl must_return nenv in
          (SBlock checked_stmts, if env.actor_init then cnenv else env)
    | Expr e   -> (SExpr (check_expr e env), env)
    | Return e ->
        let texp = check_expr e env in
          (match env.return_t with
              Some rt ->
                if (types_equal (snd texp) rt) then
                  (SReturn texp, env)
                else
                  raise (Failure ("Return type mismatch: expected " ^
                    str_types rt ^ " but returns " ^ str_types (snd texp)))
            | _ -> raise (Failure "This function needs to return"))
    | Vdecl vdecl    -> check_vdecl vdecl env
    | Mutdecl mvdecl -> check_mvdecl mvdecl env
    | Fdecl fdecl    -> check_func_decl fdecl env
    | If (cond, isl, esl) ->
        let texp = check_expr cond env in
          (match snd texp with
              Bool_t ->
                let (check_if, _) = check_stmt isl env in
                let (check_esl, _) = check_stmt esl env in
                  (SIf(texp, check_if, check_esl), env)
            | _ -> raise (Failure "Condition must be a boolean"))
    | Actor_send (e1, e2)->
        let texp1 = check_expr e1 env and texp2 = check_expr e2 env in
        let (se1, st1) = texp1 and (se2, st2) = texp2 in
          (match st1, st2 with
              Message_t m_name, Unit_t when (
                match se2 with
                    SId "sender" -> true
                  | _ -> false)  -> (SActor_send (texp1, texp2), env)
            | Message_t m_name, Actor_t a_name ->
                let sm = find_message m_name env in
                let sm_allowed = (find_actor_scope a_name env).a_messages in
                  (try
                    let _ = List.find (fun allowed_m ->
                      allowed_m.sm_name = sm.sm_name
                    ) sm_allowed in
                    (SActor_send (texp1, texp2), env)
                  with Not_found ->
                    raise (Failure ("No matching pattern to receive message " ^
                      m_name ^ " in actor " ^ a_name)))
            | _ -> raise (Failure ("Invalid send operation between " ^
                     str_types st1 ^ " to " ^ str_types st2)))
    | Pool_send (e1, e2) ->
        let texp1 = check_expr e1 env and texp2 = check_expr e2 env in
        let (se1, st1) = texp1 and (se2, st2) = texp2 in
          (match st1, st2 with
              Message_t m_name, Unit_t when (
                match se2 with
                    SId "sender" -> true
                  | _ -> false)  ->
                      raise (Failure ("Messages cannot be broadcasted to " ^
                        "sender refs"))
            | Message_t m_name, Pool_t a_name ->
                let sm = find_message m_name env in
                let sm_allowed = (find_actor_scope a_name env).a_messages in
                  (try
                    let _ = List.find (fun allowed_m ->
                      allowed_m.sm_name = sm.sm_name
                    ) sm_allowed in
                    (SPool_send (texp1, texp2), env)
                  with Not_found ->
                    raise (Failure ("No matching pattern to receive " ^
                      "message " ^ m_name ^ " in pool of actor type " ^
                        a_name)))
            | _ -> raise (Failure ("Invalid broadcast operation between " ^
                     str_types st1 ^ " to " ^ str_types st2)))

and check_vdecl (vdecl : val_decl) (env : scope) =
  let {v_name; v_type; v_init} = vdecl in
    (match v_type with
        Lambda_t (_, _) ->
          raise (Failure ("Cannot declare lambda types " ^ v_name))
      | _ ->
          let texp = check_expr v_init env in
          let (se, t) = texp in
            match se with
                SNoexpr -> raise (Failure ("Must initialize value " ^ v_name))
              | _ ->
                  if types_equal' t v_type then
                    (if name_taken v_name env then
                      raise (Failure ("Value " ^ v_name ^ " declared already"))
                    else
                      let svdecl = {
                        sv_name = v_name;
                        sv_type = v_type;
                        sv_init = texp
                      } in
                      let nv_table = upd_vtable_vals svdecl env.env_vtable in
                        (SVdecl svdecl, upd_env_vtable nv_table env))
                  else
                    raise (Failure ("Value initialization type mismatch: " ^
                      v_name ^ " is " ^ (str_types v_type) ^ " but " ^
                       "initialized as " ^ (str_types t))))

and check_mvdecl (mvdecl : mvar_decl) (env : scope) =
  if env.in_actor then
    let {mv_name; mv_type; mv_init} = mvdecl in
      (match mv_type with
          Lambda_t (_, _) ->
            raise (Failure ("Cannot declare lambda types " ^ mv_name))
        | Actor_t _ | Pool_t _ ->
            raise (Failure ("Cannot spawn mutable actor/pool " ^ mv_name))
        | _ ->
            let texp = check_expr mv_init env in
            let (se, t) = texp in
            let se_t = (match se with
                SNoexpr -> mv_type
              | _ -> t
            ) in
            if types_equal se_t mv_type then
              (if name_taken mv_name env then
                raise (Failure ("Mutable var " ^ mv_name ^ " declared already"))
              else
                let smvdecl = {
                  smv_name = mv_name;
                  smv_type = mv_type;
                  smv_init = texp;
                } in
                let nm_table = upd_mvtable_vals smvdecl env.env_mvtable in
                  (SMutdecl smvdecl, upd_env_mvtable nm_table env))
            else
              raise (Failure ("Variable initialization type mismatch: " ^
                mv_name ^ " is " ^ (str_types mv_type) ^
                  " but initialized as " ^ (str_types t))))
  else
    raise (Failure ("Mutables types are only allowed in actors"))

and check_func_decl (fdecl : func) (env : scope) =
  let { f_name; f_formals; f_return_t; f_body } = fdecl in
    try
      try
        let t = find_vtype f_name env in
        raise (Failure ("Function name " ^ f_name ^ "taken already by " ^
          "an identifier of type " ^ str_types t))
      with Not_found ->
        let param_types = get_list_snd f_formals in
          if validate_arg_types param_types && not (is_type_apm f_return_t) then
            let _ = List.find (fun sf ->
              sf.sf_name = f_name &&
                check_args_t (get_list_snd sf.sf_formals) param_types
            ) env.funcs in
            raise (Failure ("Function " ^ f_name ^ " with signature (" ^
              str_types_list param_types ^ ") => " ^ str_types f_return_t ^
                " declared already"))
          else
            raise (Failure ("Invalid function parameter/return type: " ^
              "Actors, pools and messages cannot be used as function " ^
                "arguments or return types"))
    with Not_found ->
      let forward_decl = build_func f_name f_formals f_return_t empty_fbody in
      let (nvals, nfuncs) =
        spread_arg f_formals [] (forward_decl :: env.funcs) env in
      let nv_table = { vparent = Some env.env_vtable; svals = nvals } in
      let fenv = { env with
        funcs = nfuncs;
        env_vtable = nv_table;
        return_t = Some f_return_t
      } in
      let (cfbody, _) = check_stmt f_body fenv in
      let sfdecl = build_func f_name f_formals f_return_t cfbody in
      let nenv = upd_env_funcs sfdecl env in
        (SFdecl sfdecl, nenv)

and check_stmt_list (sl : stmt list) (ret : bool) (env : scope) =
  let rec check_f_return (sll : stmt list) =
    try
      let _ = List.find (fun s -> match s with
          Return _ -> true
        | If(_, Block(b1), Block(b2)) ->
            check_f_return b1 || check_f_return b2
        | _ -> false) sll in true
    with Not_found -> false in

  let _ = (
    if ret && not (check_f_return sl) then
      raise (Failure ("This function must return " ^
        (match env.return_t with
            Some t -> str_types t
          | None -> ""
        )))
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

  let check_patterns (receive : pattern list) (senv : scope) =
    List.map (fun p ->
      let (pvals, pfuncs) = spread_arg p.p_mformals [] senv.funcs env in
      let pv_table = { vparent = Some senv.env_vtable; svals = pvals } in
      let penv = { senv with
        funcs = pfuncs;
        env_vtable = pv_table;
        actor_init = false;
      } in {
        sp_smid = p.p_mid;
        sp_smformals = p.p_mformals;
        sp_body = fst (check_stmt p.p_body penv)
      }
    ) receive in

  let { a_name; a_formals; a_body; a_receive } = adecl in
    (try
      let _ = List.find (fun ascope ->
        ascope.a_actor.sa_name = a_name
      ) env.actors in
      raise (Failure ("Actor " ^ adecl.a_name ^ " declared already"))
    with Not_found ->
      let rec check_dup l =
        (match l with
            [] -> false
          | (h :: t) ->
              let x = (List.filter (fun x -> x = h) t) in
                if (x = []) then
                  check_dup t
                else
                  true
        ) in
      if check_dup (List.map(fun p -> p.p_mid) a_receive) then
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
          let (nsvals, nsfuncs) =
            spread_arg a_formals ([]) env.funcs env in
          let nv_table = { vparent = Some env.env_vtable; svals = nsvals } in
          let curr_scope = { env with
            env_vtable  = nv_table;
            funcs       = nsfuncs;
            in_actor    = true;
            actor_init  = true
          } in
          let (checked_body, sa_env) = check_stmt a_body curr_scope in
          let sareceive = check_patterns a_receive sa_env in
          let new_sactor = {
            sa_name    = a_name;
            sa_formals = a_formals;
            sa_body    = checked_body;
            sa_receive = sareceive
          } in
          let new_actor_scope = {
            a_actor = new_sactor;
            a_scope = sa_env;
            a_messages = m_allowed
          } in
          let nenv = { env with actors = new_actor_scope :: env.actors } in
            (new_actor_scope, nenv))

let check_program (p : program) (slib : program) =
  let (messages, actors, functions) = p
  and (sl_messages, sl_actors, sl_functions) = slib in

  let sender_ref =  {
    sv_name = "sender";
    sv_type = Unit_t;
    sv_init = (SNoexpr, Unit_t);
  } in
  let die = build_func "die" [] Unit_t empty_fbody in
  let empty_vsymtab = { vparent = None; svals = sender_ref :: [] } in
  let empty_mvsymtab = { mvparent = None; smvars =  [] } in
  let seed_env = {
    messages = [];
    actors = [];
    funcs = die :: [];
    env_vtable = empty_vsymtab;
    env_mvtable = empty_mvsymtab;
    return_t = None;
    in_actor = false;
    actor_init = false;
  } in

  let (sl_smessages, sl_m_env) = List.fold_left (fun acc m ->
    let (sl_smessage, sl_nenv) = check_message_decl m (snd acc) in
    (sl_smessage :: fst acc, sl_nenv)
  ) ([], seed_env) sl_messages in
  let (sl_sactors, sl_a_env) = List.fold_left (fun acc a ->
    let (sl_a_scope, sl_nenv) = check_actor_decl a (snd acc) in
      (sl_a_scope.a_actor :: fst acc, sl_nenv)
  ) ([], sl_m_env) sl_actors in
  let (sl_sfunctions, _) = List.fold_left (fun acc f ->
    let (sl_sfunc, sl_nenv) = check_func_decl f (snd acc) in
      match sl_sfunc with
          SFdecl sf -> (sf :: fst acc, sl_nenv)
        | _ -> raise (Failure ("Not a valid function: " ^ f.f_name))
  ) ([], sl_a_env) sl_functions in

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

  let smessages = sl_smessages @ smessages in
  let sactors = sl_sactors @ sactors in
  let sfunctions = sl_sfunctions @ sfunctions in

  let main_cnt = List.fold_left (fun acc sf -> if sf.sf_name = "main"
                                  then acc + 1 else acc) 0 sfunctions in
  match main_cnt with
      0 -> raise (Failure "No main function found in this program")
    | 1 -> (List.rev smessages, List.rev sactors, List.rev sfunctions)
    | n -> raise (Failure (string_of_int n ^ " main functions found") )
