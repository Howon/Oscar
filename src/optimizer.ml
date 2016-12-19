open Ast
open Sast

open Hashtbl

type vscope = {
  loc       : string list;
  block_cnt : int;
  if_cnt    : int;
  func_cnt  : int
}

let vals = Hashtbl.create 200

let mvars = Hashtbl.create 200

(* helpers for optional stuff *)
let option_is_some o =
  match o with
      Some _  -> true
    | None    -> false

let option_is_none o =
  match o with
      Some _  -> false
    | None    -> true

let option_get o =
  match o with
      Some v  -> v
    | None    -> raise (Failure "tried to get None value")

(* helpers for variable tracking *)

let rec find_val tbl loc s =
  match List.length loc with
      0 -> None
    | _ -> let name = (String.concat "_" loc) ^ "__" ^ s in
            try Some (Hashtbl.find tbl name) with
            | Not_found -> find_val tbl (List.tl loc) s

let build_name scope s =
  (String.concat "_" scope.loc) ^ "__" ^ s

let is_replaceable scope id =
  let match_opt = find_val vals scope.loc id in
  (match match_opt with
      Some (_, rep, _) -> rep
    | None -> false
  )

let add_mvar scope decl =
  let name = build_name scope decl.smv_name in
  Hashtbl.add mvars name ((SNoexpr, Unit_t), false, 0)

let add_val scope decl =
  let {sv_name = id ; sv_type = _ ; sv_init = init} = decl in
  let name = build_name scope id in
  let (sinit, rep) =
    (match (fst init) with
        SInt_Lit _  | SDouble_Lit _ | SChar_Lit _ | SString_Lit _
      | SBool_Lit _ |  SUnit_Lit _  | SId _       -> (init, true)
      (* save space on things we won't fill in later *)
      | _ -> ((SNoexpr, Unit_t), false)
    ) in
  Hashtbl.add vals name (sinit, rep, 0)

let incr_cnt scope id =
  let name = build_name scope id in
  let val_tup = find_val vals scope.loc id in
  match val_tup with
      Some (i, r, c) -> Hashtbl.replace vals name (i, r, c + 1)
    | None ->
        let mvar_tup = find_val mvars scope.loc id in
        let (init, rep, cnt) =
          (match mvar_tup with
              Some (i, r, c) -> (i, r, c)
            | None -> ((SNoexpr, Unit_t), false, 0)
          ) in
        Hashtbl.replace mvars name (init, rep, cnt + 1)

let get_init scope id =
  match (find_val vals scope.loc id) with
      Some (i, _, _) -> i
    | None -> raise (Failure ("no init for name " ^ (build_name scope id)))

let get_cnt scope id =
  let val_tup = find_val vals scope.loc id in
  match val_tup with
      Some (_, _, c) -> c
    | None ->
        let mvar_tup = find_val mvars scope.loc id in
        match mvar_tup with
            Some (_, _, c) -> c
          | None ->
              raise (Failure ("no count for name " ^ (build_name scope id)))

(* optimize a single expression
 * IDs get replaced if they are just a simple value
 * funcs get optimized
 * all actuals used to call anything are optimized
 * uops / binops get reduced if possible *)
let rec opt_expr scope (te : t_expr) =
  let (e, t) = te in
  let ne = (match e with
      SId id ->
        let () = incr_cnt scope id in
        if is_replaceable scope id then
          fst (get_init scope id)
        else
          e
    | SAccess (e1, e2) -> SAccess((opt_expr scope e1), (opt_expr scope e2))
    | SFunc_Lit sf -> opt_func_lit scope sf
    | SList_Lit (typ, exprs) ->
        let nexprs = List.map (opt_expr scope) exprs in
        SList_Lit(typ, nexprs)
    | SSet_Lit (typ, exprs) ->
        let nexprs = List.map (opt_expr scope) exprs in
        SSet_Lit(typ, nexprs)
    | SMap_Lit (t1, t2, kvs) ->
        let nkvs = List.map (fun (k, v) ->
            (opt_expr scope k, opt_expr scope v)) kvs in
        SMap_Lit(t1, t2, nkvs)
    | SMessage_Lit (id, exprs) ->
        let nexprs = List.map (opt_expr scope) exprs in
        SMessage_Lit (id, nexprs)
    | SActor_Lit (id, exprs)->
        let nexprs = List.map (opt_expr scope) exprs in
        SActor_Lit (id, nexprs)
    | SPool_Lit (id, exprs, cnt) ->
        let nexprs = List.map (opt_expr scope) exprs in
        let ncnt = opt_expr scope cnt in
        SPool_Lit (id, nexprs, ncnt)
    | SBinop (e1, op, e2) -> opt_binop scope e1 op e2
    | SUop (uop, e1) -> opt_uop scope uop e1
    | SFuncCall (id, exprs) ->
        let () = incr_cnt scope id in
        let nexprs = List.map (opt_expr scope) exprs in
        SFuncCall (id, nexprs)
    | _ -> e
  ) in
  (ne, t)

(* optimize the body of the function *)
and opt_func_lit scope (f : sfunc) =
  let fst_body = (opt_outer_block opt_stmt) scope f.sf_body in
  let snd_body = (opt_outer_block snd_stmt) scope fst_body in
  SFunc_Lit({ f with sf_body = snd_body })

(* reduce uops of expressions that can be reduced to literals *)
and opt_uop scope (uop: u_op) (e : t_expr) =
  let (e', t) = opt_expr scope e in
  match uop with
      Not ->
        (match e' with
            SBool_Lit b   -> SBool_Lit (not b)
          | _ -> SUop(uop, (e',t))
        )
    | Neg ->
        (match e' with
            SInt_Lit i    -> SInt_Lit (-i)
          | SDouble_Lit d -> SDouble_Lit (-.d)
          | _ -> SUop(uop, (e',t))
        )

(* reduce binops of expressions that can be reduced to literals *)
and opt_binop scope (e1 : t_expr) (op : bin_op) (e2 : t_expr) =
  let (e1', t1) = opt_expr scope e1 in
  let (e2', t2) = opt_expr scope e2 in
  match op with
      Add ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2        -> SInt_Lit (i1 + i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SDouble_Lit (d1 +. d2)
          | SString_Lit s1, SString_Lit s2  -> SString_Lit (s1 ^ s2)
          | _ -> SBinop((e1',t1), op, (e2',t2))
        )
    | Sub   ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 - i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SDouble_Lit(d1 -. d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Mult  ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 * i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SDouble_Lit(d1 *. d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Div   ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 / i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SDouble_Lit(d1 /. d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Less  ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 < i2)
          | SDouble_Lit d1, SDouble_Lit d2  ->
              SBool_Lit ((compare d1 d2) < 0)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Leq ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 <= i2)
          | SDouble_Lit d1, SDouble_Lit d2  ->
              SBool_Lit ((compare d1 d2) <= 0)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Greater ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 > i2)
          | SDouble_Lit d1, SDouble_Lit d2  ->
              SBool_Lit ((compare d1 d2) > 0)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Geq ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 >= i2)
          | SDouble_Lit d1, SDouble_Lit d2  ->
              SBool_Lit ((compare d1 d2) >= 0)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Mod ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 mod i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Bit_And ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 land i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Bit_Or ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 lor i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Bit_Xor ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 lxor i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Bit_RShift ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 lsr i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Bit_LShift ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SInt_Lit (i1 lsl i2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Equal ->
        (match e1', e2' with
            SInt_Lit v1, SInt_Lit v2            -> SBool_Lit (v1 = v2)
          | SDouble_Lit v1, SDouble_Lit v2      ->
              SBool_Lit ((compare v1 v2) = 0)
          | SChar_Lit v1, SChar_Lit v2          -> SBool_Lit (v1 = v2)
          | SString_Lit v1, SString_Lit v2      -> SBool_Lit (v1 = v2)
          | SBool_Lit v1, SBool_Lit v2          -> SBool_Lit (v1 = v2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Neq ->
        (match e1', e2' with
            SInt_Lit v1, SInt_Lit v2            -> SBool_Lit (v1 != v2)
          | SDouble_Lit v1, SDouble_Lit v2      ->
              SBool_Lit ((compare v1 v2) != 0)
          | SChar_Lit v1, SChar_Lit v2          -> SBool_Lit (v1 != v2)
          | SString_Lit v1, SString_Lit v2      -> SBool_Lit (v1 != v2)
          | SBool_Lit v1, SBool_Lit v2          -> SBool_Lit (v1 != v2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | And ->
        (match e1', e2' with
            SBool_Lit b1, SBool_Lit b2    -> SBool_Lit(b1 && b2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Or ->
        (match e1', e2' with
            SBool_Lit b1, SBool_Lit b2    -> SBool_Lit(b1 || b2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Assign ->
        SBinop((e1', t1), op, (e2', t2))

(* optimize a statement, returning Some sstmt if useful else None
 * cleans up all statements and expressions
 * ignores variable declaration if the variable init value can be
 * reduced to a literal or an id of another variable *)
and opt_stmt scope (s : sstmt) =
  match s with
      SBlock stmts -> opt_block opt_stmt scope stmts
    | SExpr (e, t) ->
        (match e with
            SFuncCall _ ->
              let nexpr = opt_expr scope (e, t) in
              (Some (SExpr(nexpr)), scope)
          | SBinop (_, Assign, _) ->
              let nexpr = opt_expr scope (e, t) in
              (Some (SExpr(nexpr)), scope)
          | _ -> (None, scope)
        )
    | SReturn e ->
        let nexpr = opt_expr scope e in
        (Some (SReturn(nexpr)), scope)
    | SVdecl vd -> opt_valdecl scope vd
    | SMutdecl md -> opt_mutdecl scope md
    | SIf (spred, sif, selse) -> opt_if scope spred sif selse
    | SActor_send (e1, e2) ->
        let nmess = opt_expr scope e1 in
        let nact = opt_expr scope e2 in
        (Some (SActor_send(nmess, nact)), scope)
    | SPool_send (e1, e2) ->
        let nmess = opt_expr scope e1 in
        let npool = opt_expr scope e2 in
        (Some (SPool_send(nmess, npool)), scope)

(* handling the "inner" blocks that can be nested *)
and opt_block stmt_func scope stmts =
  let nscope =
    if scope.block_cnt = 0 then
      { scope with block_cnt = 1 }
    else
      let new_name = "b" ^ (string_of_int scope.block_cnt) in
      { loc = new_name :: scope.loc; block_cnt = 0;
          if_cnt = 0; func_cnt = 0 }
    in
  let acc_fun (scope, slist) stmt =
    let (nstmt, nscope) = stmt_func scope stmt in
    if option_is_some nstmt then
      (nscope, (option_get nstmt) :: slist)
    else
      (nscope, slist)
  in
  let (_, nstmts) = List.fold_left acc_fun (nscope, []) stmts in

  let ret_scope = { scope with block_cnt = scope.block_cnt + 1 } in

  (match nstmts with
      []  -> (None, ret_scope)
    | _   -> ((Some (SBlock (List.rev nstmts))), ret_scope)
  )

(* this is to handle "higher-level" blocks that always must exist *)
and opt_outer_block stmt_func scope block =
  let stmts =
    (match block with
      SBlock s -> s
    | _ -> raise (Failure "opt_inner_block called without block")
    ) in
  let o_block = fst (opt_block stmt_func scope stmts) in
  if option_is_some o_block then
    option_get o_block
  else
    SBlock([])

(* optimize immutable variable declaration
 * if we save the value of the variable, don't actually put the
 * declaration in code *)
and opt_valdecl scope vd =
  let opt_vd = { vd with sv_init = opt_expr scope vd.sv_init } in
  let () = add_val scope opt_vd in
  let nstmt =
    (if (is_replaceable scope vd.sv_name) then
      None
    else
      Some (SVdecl (opt_vd) )
    ) in
  (nstmt, scope)

(* optimize mutable varialbe declaration
 * always ends up in code, but init value is optimize *)
and opt_mutdecl scope md =
  let () = add_mvar scope md in
  let opt_md = { md with smv_init = opt_expr scope md.smv_init } in
  (Some ( SMutdecl (opt_md) ), scope)


(* optimize if/else block
 * if we can reduce the predicate to a boolean literal
 * then replace the if with a block of the used part *)
and opt_if scope pred ifb elseb =
  let clean_scope scope name =
    let new_name =
      (if scope.if_cnt = 0 then
        name
      else
        (name ^ (string_of_int scope.if_cnt))
      ) in
    { loc = new_name :: scope.loc; block_cnt = 0; if_cnt = 0; func_cnt = 0 }
  in

  let (ne, nt) = opt_expr scope pred in

  let nstmt = (match ne with
      SBool_Lit b ->
        (if b then
          fst (opt_stmt (clean_scope scope "if") ifb)
        else
          fst (opt_stmt (clean_scope scope "else") elseb)
        )
    | _ ->
        let nifb_o = fst(opt_stmt (clean_scope scope "if") ifb) in
        let nelseb_o = fst(opt_stmt (clean_scope scope "else") elseb) in
        if option_is_none nifb_o && option_is_none nelseb_o then
          None
        else
          let nifb =
            (if option_is_some nifb_o then
              option_get nifb_o
            else
              SExpr((SNoexpr, Unit_t))
            ) in
          let nelseb =
            (if option_is_some nelseb_o then
              option_get nelseb_o
            else
              SExpr((SNoexpr, Unit_t))
            ) in
          Some (SIf((ne, nt), nifb, nelseb)))
  in
  (nstmt, { scope with if_cnt = scope.if_cnt + 1 })

and snd_stmt scope s =
  match s with
      SBlock stmts -> opt_block snd_stmt scope stmts
    | SVdecl vd -> snd_vdecl scope vd
    | SMutdecl md -> snd_mdecl scope md
    | SIf (spred, sif, selse) -> snd_if scope spred sif selse
    | _ -> (Some s, scope)

and snd_vdecl scope vd =
  if (get_cnt scope vd.sv_name) = 0 then
    (None, scope)
  else
    (Some (SVdecl vd), scope)

and snd_mdecl scope md =
  if (get_cnt scope md.smv_name) = 0 then
    (None, scope)
  else
    (Some (SMutdecl md), scope)

and snd_if scope spred ifb elseb =
  let clean_scope scope name =
    let new_name =
      (if scope.if_cnt = 0 then
        name
      else
        (name ^ (string_of_int scope.if_cnt))
      ) in
    { loc = new_name :: scope.loc; block_cnt = 0; if_cnt = 0; func_cnt = 0 }
  in
  let nifb = opt_outer_block snd_stmt (clean_scope scope "if") ifb in
  let nelseb = opt_outer_block snd_stmt (clean_scope scope "else") elseb in
  (Some (SIf(spred, nifb, nelseb)), { scope with if_cnt = scope.if_cnt + 1 })

let opt_funcdecl scope f =
  let () = add_val scope f in
  let nscope = { loc = f.sv_name :: scope.loc; block_cnt = 0;
                  if_cnt = 0; func_cnt = 0 } in
  { f with sv_init = opt_expr nscope f.sv_init }

let snd_funcdecl scope f =
  if (get_cnt scope f.sv_name) = 0 then
    None
  else
    Some f

(* optimize a pattern: reduce the body *)
let opt_pattern scope spat =
  let nscope = { scope with loc = spat.sp_smid :: scope.loc } in
  let nbody = (opt_outer_block opt_stmt) nscope spat.sp_body in
  { spat with sp_body = nbody }

(* optimize an actor: reduce the body and optimize all patterns *)
let opt_actor scope sact =
  let nscope = { loc = sact.sa_name :: scope.loc;
                  block_cnt = 0; if_cnt = 0; func_cnt = 0 } in

  let nbody = (opt_outer_block opt_stmt) nscope sact.sa_body in
  let nreceive = List.map (opt_pattern nscope) sact.sa_receive in
  { sact with sa_body = nbody; sa_receive = nreceive }


let optimize_program (messages, actors, functions, main) =
  let main_scope =
    { loc = ["main"]; block_cnt = 0; if_cnt = 0; func_cnt = 0 } in
  let fp_actors = List.map (opt_actor main_scope) actors in
  let fp_functions = List.map (opt_funcdecl main_scope) functions in
  let (main_lit, main_typ) = main in
  let fp_main = match (main_lit) with
        SFunc_Lit(f) -> opt_func_lit main_scope f
      | _ -> raise (Failure "bad main") in

  let sp_functions = List.map option_get (List.filter option_is_some
      (List.map (snd_funcdecl main_scope) fp_functions)) in
  (messages, fp_actors, sp_functions, (fp_main, main_typ))
