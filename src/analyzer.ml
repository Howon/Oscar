open Ast
open Sast
open Exception

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
    vsymbol_table : vsymtab;
    mvsymbol_table : mvsymtab;
    return_type : types option;
    funcs : sfunc list;
    in_loop : bool
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
            | _ ->
                raise Exception.Duplicate_Decl
    with Not_found ->
        try
            let mvtype = find_variable_decl mvstab name in mvtype.mv_type
        with Not_found -> raise Not_found

let check_binop (te1 : t_expr) (te2 : t_expr) (op : bin_op) (env : environment) =
    let (e1, t1) = te1 and (e2, t2) = te2 in
    match op with
        Add ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
            | String_t, String_t -> (SBinop(e1, op, e2), String_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | Sub | Mult | Div | Neq | Less | Leq | Greater | Geq ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | Double_t, Double_t -> (SBinop(e1, op, e2), Double_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                             " string_of_bin_op op")))
      | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
          (match t1, t2 with
              Int_t, Int_t  -> (SBinop(e1, op, e2), Int_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                             " string_of_bin_op op")))
      | Equal | Neq ->
          (match t1, t2 with
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
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | And | Or ->
          (match t1, t2 with
              Bool_t, Bool_t -> (SBinop(e1, op, e2), Bool_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | Assign ->
          (match e1 with
              SId s when t1 = t2 ->
                  (if (is_immu env.vsymbol_table s) then
                      raise (Invalid_argument("Reassignment to a value " ^ s))
                  else (SBinop(e1, op, e2), t1))
            | _ -> raise (Invalid_argument("Assignment to invalid id")))
      | Access ->
          (match t1, t2 with
              List_t(lt), Int_t -> (SBinop(e1, op, e2), lt)
            | Map_t(kt, vt), kt1 ->  (SBinop(e1, op, e2), vt)
            | Set_t(st), _ when st = t2 -> (SBinop(e1, op, e2), st)
            | _ -> raise (Invalid_argument("Invalid access types")))

let check_uop (te : t_expr) (op : u_op) =
    let (e, t) = te in
    match op with
        Neg ->
           (match t with
              Int_t  -> (SUop(op, e), Int_t)
            | Double_t -> (SUop(op, e), Double_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                                      " string_of_u_op op")))
      | Not ->
           (match t with
              Bool_t -> (SUop(op, e), Bool_t)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                                      " string_of_u_op op")))
let check_func_call (fname : string) (params: t_expr list) (env: environment) =
    try
        let fentry = List.find (fun sf -> sf.sf_name = fname) env.funcs in
        let required_params = List.map (fun (_, x) -> x) fentry.sf_formals in
        let actual_params = List.map (fun (_, x) -> x) params in
        if (actual_params <> required_params) then
            raise (Invalid_argument ("Function called with wrong parameter types"))
        else (SCall(fname, params), fentry.sf_return_t)
    with Not_found -> raise (Failure ("Unknown function " ^ fname))

let check_actor_comm (m : message) (act_op : actor_op) (target : actor_type) =
    let m_accepted = List.exists (fun p ->
        (p.p_message_id = m.m_name) && (p.p_message_formals = m.m_formals)) in
    if m_accepted then (SActor_comm(m, act_op, actor_type))

let rec check_expr (e : expr) (env : environment) =
    match e with
          Binop(e1, op, e2) ->
            let checked_e1 = check_expr e1 env and
                checked_e2 = check_expr e2 env in
                check_binop checked_e1 checked_e2 op env
        | Uop(op, e) ->
            let checked_e = check_expr e env in
                check_uop checked_e op
        | Id id ->
            try
                let vtype = find_vtype env.vsymbol_table env.mvsymbol_table id in
                (SId id, vtype)
            with Not_found ->
                raise (Failure ("Undeclared identifier " ^ id))
        | Int_Lit i -> (SInt i, Int_t)
        | Double_Lit d -> (Double_Lit d, Double_t)
        | Char_Lit c -> (SChar_Lit c, Char_t)
        | String_t s -> (SString_Lit s, String_t)
        | Bool_Liti b -> (SBool_Lit b, Bool_t)
        | Unit_Lit u -> (SUnit_Lit u, Unit_t)
        | List_Lit(lt, ex) -> (SList_Lit(lt, ex), List_t lt)
        | Set_Lit(st, ex) -> (SSet_Lit(st, ex), Set_t st)
        | Map_Lit(kt, vt, ex) -> (SMap_Lit(kt, vt, ex), Map_t(kt, vt))
        | Lambda ld -> (SLambda ld, ld.l_return_t)
        | Call(f_id, args) ->
            check_func_call f_id (List.map (fun exp -> check_expr exp env) args) env
        | Actor_comm(m, act_op, act) ->





