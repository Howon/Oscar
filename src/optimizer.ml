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
let build_name scope s =
  (String.concat "_" scope.loc) ^ "__" ^ s

let is_replaceable scope id =
  let name = build_name scope id in
  let is_immut = Hashtbl.mem vals name in
  if not is_immut then
    false
  else
    let (init, typ, cnt) = Hashtbl.find vals name in
    match typ with
        Int_t | Bool_t | Double_t | Char_t | Unit_t -> true
      | _ -> false

let add_mvar scope decl =
  let name = build_name scope decl.smv_name in
  Hashtbl.add mvars name ((SNoexpr, Unit_t), decl.smv_type, 0)

let add_val scope decl =
  let name = build_name scope decl.sv_name in
  (* there is no reason to waste space on stuff we won't fill in *)
  let init = (match decl.sv_type with
        Int_t | Bool_t | Double_t | Char_t | Unit_t -> decl.sv_init
      | _ -> (SNoexpr, Unit_t)
  ) in
  Hashtbl.add vals name (init, decl.sv_type, 0)

let incr_cnt scope id =
  let name = build_name scope id in
  if Hashtbl.mem mvars name then
    let (init, typ, cnt) = Hashtbl.find mvars name in
    Hashtbl.replace mvars name (init, typ, cnt + 1)
  else
    let (init, typ, cnt) = Hashtbl.find vals name in
    Hashtbl.replace vals name (init, typ, cnt + 1)

let get_init scope id =
  let name = build_name scope id in
  let (init, typ, cnt) = Hashtbl.find vals name in
  init

let get_cnt scope id =
  let name = build_name scope id in
  let (_, _, cnt) =
    (if Hashtbl.mem mvars name then
      Hashtbl.find mvars name
    else
      Hashtbl.find vals name
    ) in
  cnt

let rec opt_expr scope (te : t_expr) =
  let (e, t) = te in
  let ne = (match e with
      SId id ->
        let () = incr_cnt scope id in
        if is_replaceable scope id then
          fst (get_init scope id)
        else
          e
    | SAccess (e1, e2) -> SAccess(e1, (opt_expr scope e2))
    | SFunc_Lit sf -> opt_func_lit scope sf
    | SList_Lit (typ, exprs)
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

and opt_func_lit scope (f : sfunc) =
  SFunc_Lit({ f with sf_body = (opt_block_always scope f.sf_body) })

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
          | SDouble_Lit d1, SDouble_Lit d2  -> SBool_Lit(d1 < d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Leq ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 <= i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SBool_Lit(d1 <= d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Greater ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 > i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SBool_Lit(d1 > d2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Geq ->
        (match e1', e2' with
            SInt_Lit i1, SInt_Lit i2      -> SBool_Lit (i1 >= i2)
          | SDouble_Lit d1, SDouble_Lit d2  -> SBool_Lit(d1 >= d2)
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
          | SDouble_Lit v1, SDouble_Lit v2      -> SBool_Lit (v1 = v2)
          | SChar_Lit v1, SChar_Lit v2          -> SBool_Lit (v1 = v2)
          | SString_Lit v1, SString_Lit v2      -> SBool_Lit (v1 = v2)
          | SBool_Lit v1, SBool_Lit v2          -> SBool_Lit (v1 = v2)
          | _ -> SBinop((e1', t1), op, (e2', t2))
        )
    | Neq ->
        (match e1', e2' with
            SInt_Lit v1, SInt_Lit v2            -> SBool_Lit (v1 != v2)
          | SDouble_Lit v1, SDouble_Lit v2      -> SBool_Lit (v1 != v2)
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

(* optimize a statement, returning Some sstmt if useful else None *)
and opt_stmt scope (s : sstmt) =
  match s with
      SBlock stmts -> opt_block scope stmts
    | SExpr (e, t) ->
        (match e with
            SFuncCall _ ->
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
        (Some (SActor_send(nmess, npool)), scope)

(* handling the "lower-level" blocks that can be nested *)
and opt_block scope stmts =
  let nscope =
    if scope.block_cnt = 0 then
      { scope with block_cnt = 1 }
    else
      let new_name = "b" ^ (string_of_int scope.block_cnt) in
      { loc = new_name :: scope.loc; block_cnt = 0;
          if_cnt = 0; func_cnt = 0 }
    in
  let acc_fun (nscope, slist) stmt =
    let (nstmt, nscope) = opt_stmt scope stmt in
    if option_is_some nstmt then
      (nscope, (option_get nstmt) :: slist)
    else
      (nscope, slist)
  in
  let (nscope, nstmts) = List.fold_left acc_fun (scope, []) stmts in

  let ret_scope = { scope with block_cnt = scope.block_cnt + 1 } in

  (match nstmts with
      []  -> (None, ret_scope)
    | _   -> ((Some (SBlock (List.rev nstmts))), ret_scope)
  )


(* this is to handle "higher-level" blocks that always must exist *)
and opt_block_always scope block =
  let stmts =
    (match block with
      SBlock s -> s
    | _ -> raise (Failure "opt_block called without block")
    ) in
  let o_block = fst (opt_block scope stmts) in
  if option_is_some o_block then
    option_get o_block
  else
    SBlock([])

and opt_valdecl scope vd =
  let () = add_val scope vd in
  let nstmt =
    (if (is_replaceable scope vd.sv_name) then
      None
    else
      let opt_vd = { vd with sv_init = opt_expr scope vd.sv_init } in
      Some (SVdecl (opt_vd) )
    ) in
  (nstmt, scope)

and opt_mutdecl scope md =
  let () = add_mvar scope md in
  let opt_md = { md with smv_init = opt_expr scope md.smv_init } in
  (Some ( SMutdecl (opt_md) ), scope)

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
  let nscope = clean_scope scope "if" in

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
              SBlock([])
            ) in
          let nelseb =
            (if option_is_some nelseb_o then
              option_get nelseb_o
            else
              SBlock([])
            ) in
          Some (SIf((ne, nt), nifb, nelseb)))
  in
  (nstmt, { scope with if_cnt = scope.if_cnt + 1 })

let opt_funcdecl scope f =
  let nscope = { loc = f.sv_name :: scope.loc; block_cnt = 0;
                  if_cnt = 0; func_cnt = 0 } in
  let () = add_val nscope f in
  { f with sv_init = opt_expr nscope f.sv_init }

let opt_pattern scope spat =
  let nscope = { scope with loc = spat.sp_smid :: scope.loc } in
  let nbody = opt_block_always nscope spat.sp_body in
  { spat with sp_body = nbody }

let opt_actor scope sact =
  let nscope = { loc = sact.sa_name :: scope.loc;
                  block_cnt = 0; if_cnt = 0; func_cnt = 0 } in

  let nbody = opt_block_always nscope sact.sa_body in
  let nreceive = List.map (opt_pattern nscope) sact.sa_receive in
  { sact with sa_body = nbody; sa_receive = nreceive }

let optimize_program (messages, actors, functions, main) =
  let main_scope =
    { loc = ["main"]; block_cnt = 0; if_cnt = 0; func_cnt = 0 } in
  let faster_actors = List.map (opt_actor main_scope) actors in
  let faster_functions = List.map (opt_funcdecl main_scope) functions in
  let (main_lit, main_typ) = main in
  let main_scope =
    { loc = ["main"]; block_cnt = 0; if_cnt = 0; func_cnt = 0 } in
  let faster_main = match (main_lit) with
        SFunc_Lit(f) -> opt_func_lit main_scope f
      | _ -> raise (Failure "bad main") in
  (messages, faster_actors, faster_functions, (faster_main, main_typ))
