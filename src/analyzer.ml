open Ast
open Sast
open Exception
open Prettyprint

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type vsymtab = {
    vparent      : vsymtab option;
    vals         : val_decl list;
}

type mvsymtab = {
    mvparent     : mvsymtab option;
    mvars        : mvar_decl list;
}

type actor_map = {
    aformals_list     : formal list;
    afunc_list        : sfunc list;
    areceive          : spattern list;
}

type environment = {
    vsymbol_table  : vsymtab;
    mvsymbol_table : mvsymtab;
    return_type    : types option;
    messages       : smessage list;
    actors         : sactor list;
    funcs          : sfunc list;
    in_loop        : bool
}

let rec find_value_decl (scope : vsymtab) (v_name : string) =
    try
        List.find (fun val_decl -> val_decl.v_name = v_name) scope.vals
    with Not_found ->
        match scope.vparent with
            Some(vparent) -> find_value_decl vparent v_name
          | _ -> raise Not_found

let rec find_variable_decl (scope : mvsymtab) (mv_name : string) =
    try
        List.find (fun mvar_decl -> mvar_decl.mv_name = mv_name) scope.mvars
    with Not_found ->
        match scope.mvparent with
            Some mvparent -> find_variable_decl mvparent mv_name
          | _ -> raise Not_found

let is_immu (scope : vsymtab) (v_name : string) =
    try
        let _ = find_value_decl scope v_name in true
    with
          Not_found -> false

let is_mut (scope : mvsymtab) (mv_name : string) =
    try
        let _ = find_variable_decl scope mv_name in true
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

let find_message (env : environment) (sm_name : string) =
    try
        List.find (fun sm -> sm_name = sm.sm_name) env.messages
    with Not_found ->
        raise (Failure ("Message of type " ^ sm_name ^ " not found"))

let find_actor (env : environment) (sa_name : string) =
    try
        List.find (fun sact -> sa_name = sact.sa_name) env.actors
    with Not_found ->
        raise (Failure ("Actor of type " ^ sa_name ^ " not found"))

let find_func (env : environment) (sf_name : string) =
    try
        List.find (fun sfun -> sf_name = sfun.sf_name) env.funcs
    with Not_found ->
        raise (Failure ("Function of name " ^ sf_name ^ " not found"))

let get_comp_texpr (op : bin_op) (e1 : sexpr) (t1 : types)
        (e2 : sexpr) (t2 : types) =
    match t1, t2 with
        Int_t, Int_t       -> (SBinop(e1, op, e2), Int_t)
      | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
      | Char_t, Char_t     -> (SBinop(e1, op, e2), Char_t)
      | String_t, String_t -> (SBinop(e1, op, e2), String_t)
      | Bool_t, Bool_t     -> (SBinop(e1, op, e2), Bool_t)
      | List_t(lt1), List_t(lt2)
          when lt1 = lt2 -> (SBinop(e1, op, e2), List_t(lt1))
      | Map_t(kt1, vt1), Map_t(kt2, vt2)
          when (kt1 = kt2) && (vt1 = vt2) ->  (SBinop(e1, op, e2), Map_t(kt1, vt1))
      | Set_t(st1), Set_t(st2)
          when st1 = st2 -> (SBinop(e1, op, e2), Set_t(st1))
      | Actor_t(_), Actor_t(_) -> raise Exception.Actor_err
      | Pool_t(_), Pool_t(_)   -> raise Exception.Pool_err
      | _ -> raise (Failure ("operand type mismatch: " ^
                            " string_of_bin_op op"))

let check_binop (te1 : t_expr) (te2 : t_expr) (op : bin_op) (env : environment) =
    let (e1, t1) = te1 and (e2, t2) = te2 in
    match op with
        Add -> (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
            | String_t, String_t -> (SBinop(e1, op, e2), String_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | Sub | Mult | Div | Neq | Less | Leq | Greater | Geq ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                             " string_of_bin_op op")))
      | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                             " string_of_bin_op op")))
      | Equal | Neq ->
          (try
              get_comp_texpr op e1 t1 e2 t2
          with
              Exception.Actor_err ->
                  raise (Failure "Actors cannot be compared for equality")
            | Exception.Pool_err  ->
                  raise (Failure "Pools cannot be compared for equality"))
      | And | Or ->
          (match t1, t2 with
              Bool_t, Bool_t -> (SBinop(e1, op, e2), Bool_t)
            | _ -> raise (Failure ("operand type mismatch: " ^
                            (str_types t1) ^ " and " ^
                            (str_types t2))))
      | Assign ->
          (match e1 with
              SId s when t1 = t2 ->
                  (if (is_immu env.vsymbol_table s) then
                      raise (Failure("Reassignment to a value " ^ s))
                  else if (is_mut env.mvsymbol_table s) then
                      try
                          get_comp_texpr op e1 t1 e2 t2
                      with
                          Exception.Actor_err ->
                              if t1 = t2 then (SBinop(e1, op, e2), t1) else
                              raise (Failure("Assignment to incompatible type"))
                        | Exception.Pool_err  ->
                              if t1 = t2 then (SBinop(e1, op, e2), t1) else
                              raise (Failure("Assignment to incompatible type"))
                  else
                      raise (Failure("Assignment to invalid id")))
            | _ -> raise (Failure("Assignment to incompatible type")))
      | Access ->
          (match t1, t2 with
              List_t(lt), Int_t             -> (SBinop(e1, op, e2), lt)
            | Map_t(kt, vt), _ when kt = t2 -> (SBinop(e1, op, e2), vt)
            | Set_t(st), _ when st = t2     -> (SBinop(e1, op, e2), Bool_t)
            | _ -> raise (Failure("Invalid access types")))

let check_uop (te : t_expr) (op : u_op) =
    let (e, t) = te in
    match op with
        Neg ->
          (match t with
              Int_t    -> (SUop(op, e), Int_t)
            | Double_t -> (SUop(op, e), Double_t)
            | _        -> raise (Failure ("operand type mismatch: " ^
                              " string_of_u_op op")))
      | Not ->
          (match t with
              Bool_t -> (SUop(op, e), Bool_t)
            | _      -> raise (Failure ("operand type mismatch: " ^
                              " string_of_u_op op")))

let check_func_call (f_entry : sfunc) (params : t_expr list) =
    let required_params = List.map (fun (_, x) -> x) f_entry.sf_formals in
    let actual_params = List.map (fun (_, x) -> x) params in
    List.iter2 (fun t1 t2 -> (match t1, t2 with
          Lambda_t(args1, rt1), Lambda_t(args2, rt2)
               when (rt1 != rt2) || (args1 <> args2) ->
                   raise Exception.Type_mismatch
        | _ -> if (t1 != t2) then raise
                    Exception.Type_mismatch
               else ()
    )) actual_params required_params;
    (SCall(f_entry.sf_name, params), f_entry.sf_return_t)

let check_actor_decl (sa_entry : sactor) (params : t_expr list) =
    let constructors = List.map (fun (_, x) -> x) sa_entry.sa_formals in
    let actual_params = List.map (fun (_, x) -> x) params in
    if (actual_params <> constructors ) then
        raise (Failure ("Actor constructed with wrong parameter types"))
    else (SActor_Lit(sa_entry.sa_name, (List.map(fun (t, _) -> t) params)), Actor_t(sa_entry.sa_name))

let rec check_expr (e : expr) (env : environment) =
    match e with
          Int_Lit i           -> (SInt_Lit i, Int_t)
        | Double_Lit d        -> (SDouble_Lit d, Double_t)
        | Char_Lit c          -> (SChar_Lit c, Char_t)
        | String_Lit s          -> (SString_Lit s, String_t)
        | Bool_Lit b         -> (SBool_Lit b, Bool_t)
        | Unit_Lit u          -> (SUnit_Lit u, Unit_t)
        | List_Lit(lt, ex)    ->
              let t_expr_list = List.map (fun t -> check_expr t env) ex in
              let sexpr_list = List.map (fun (se, _) -> se) t_expr_list in
              (SList_Lit(lt, sexpr_list), List_t lt)
        | Set_Lit(st, ex)     ->
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
            let sactor = find_actor env at in
            let te_list = List.map (fun exp -> check_expr exp env) e in
            check_actor_decl sactor te_list
        | Pool_Lit(at, e, num) ->
                let sactor = find_actor env at in
                let te_list = List.map (fun exp -> check_expr exp env) e in
                check_actor_decl sactor te_list
        | Binop(e1, op, e2) ->
            let checked_e1 = check_expr e1 env and
                checked_e2 = check_expr e2 env in
                check_binop checked_e1 checked_e2 op env
        | Uop(op, e) ->
            let checked_e = check_expr e env in
                check_uop checked_e op
        | Id id ->
            (try
                let vtype = find_vtype env.vsymbol_table env.mvsymbol_table id in
                (SId id, vtype)
            with
                Not_found ->
                    raise (Failure ("Undeclared identifier " ^ id))
              | Exception.Duplicate_decl ->
                    raise (Failure (id ^ " declared as both mutable and immutable")))
        (*| Lambda ld ->
            let formal_types = List.map (fun (_, ft) -> ft) ld.l_formals in
            (SLambda ld, Lambda_t (formal_types, l_return_t))*)
        | Call(f_id, args) ->
            try
                let sf = find_func env f_id in
                check_func_call sf (List.map (fun exp -> check_expr exp env) args)
            with
                Invalid_argument(_) ->
                    raise (Failure "Number of arguments do not match")
              | Exception.Type_mismatch ->
                    raise (Failure "Argument types do not match")





