open Ast
open Sast

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

let rec opt_expr (te : t_expr) =
  let (e, t) = te in
  let ne = (match e with
      SId id -> e
    | SAccess (e1, e2) -> SAccess(e1, (opt_expr e2))
    | SFunc_Lit sf -> SFunc_Lit (opt_func_lit sf)
    | SList_Lit (typ, exprs)
    | SSet_Lit (typ, exprs) ->
        let nexprs = List.map opt_expr exprs in
        SSet_Lit(typ, nexprs)
    | SMap_Lit (t1, t2, kvs) -> e
    | SMessage_Lit (id, exprs) ->
        let nexprs = List.map opt_expr exprs in
        SMessage_Lit (id, nexprs)
    | SActor_Lit (id, exprs)->
        let nexprs = List.map opt_expr exprs in
        SActor_Lit (id, nexprs)
    | SPool_Lit (id, exprs, cnt) ->
        let nexprs = List.map opt_expr exprs in
        let ncnt = opt_expr cnt in
        SPool_Lit (id, exprs, cnt)
    | SBinop (e1, op, e2) -> opt_binop e1 op e2
    | SUop (uop, e1) -> opt_uop uop e1
    | SFuncCall (id, exprs) ->
        let nexprs = List.map opt_expr exprs in
        SFuncCall (id, nexprs)
    | _ -> e
  ) in
  (ne, t)

and opt_func_lit (f : sfunc) =
  { f with sf_body = (opt_block f.sf_body) }

and opt_uop (uop: u_op) (e : t_expr) =
  let (e', t) = opt_expr e in
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

and opt_binop (e1 : t_expr) (op : bin_op) (e2 : t_expr) =
  let (e1', t1) = opt_expr e1 in
  let (e2', t2) = opt_expr e2 in
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
and opt_stmt (s : sstmt) =
  match s with
      SBlock stmts ->
        let nstmts = List.map (fun s -> option_get s)
          (List.filter (fun s -> option_is_some s)
            (List.map opt_stmt stmts)) in
        (match nstmts with
            []  -> None
          | _   -> Some (SBlock (nstmts))
        )
    | SExpr (e, _) ->
        (match e with
            SFuncCall _ -> Some s
          | _ -> None
        )
    | SReturn e ->
        let nexpr = opt_expr e in
        Some (SReturn(nexpr))
    | SVdecl vd -> Some (SVdecl (opt_vdecl vd))
    | SMutdecl md -> Some (SMutdecl (opt_mutdecl md))
    | SIf (spred, sif, selse) -> opt_if spred sif selse
    | SActor_send (e1, e2) ->
        let nmess = opt_expr e1 in
        let nact = opt_expr e2 in
        Some (SActor_send(nmess, nact))
    | SPool_send (e1, e2) ->
        let nmess = opt_expr e1 in
        let npool = opt_expr e2 in
        Some (SActor_send(nmess, npool))

(* this is to handle "higher-level" blocks that always must exist *)
and opt_block block =
  let stmts =
    (match block with
      SBlock s -> s
    | _ -> raise (Failure "opt_block called without block")
    ) in
  let nstmts = List.map (fun s -> option_get s)
    (List.filter (fun s -> option_is_some s)
      (List.map opt_stmt stmts)) in
  SBlock(nstmts)

and opt_vdecl vd =
  { vd with sv_init = opt_expr vd.sv_init }

and opt_mutdecl md =
  { md with smv_init = opt_expr md.smv_init }

and opt_if pred ifb elseb =
  let (ne, nt) = opt_expr pred in
  match ne with
      SBool_Lit b ->
        (if b then
          opt_stmt ifb
        else
          opt_stmt elseb)
    | _ ->
        let nifb_o = opt_stmt ifb in
        let nelseb_o = opt_stmt elseb in
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
          Some (SIf((ne, nt), nifb, nelseb))

let opt_pattern spat =
  let nbody = opt_block spat.sp_body in
  { spat with sp_body = nbody }

let opt_actor sact =
  let nbody = opt_block sact.sa_body in
  let nreceive = List.map opt_pattern sact.sa_receive in
  { sact with sa_body = nbody; sa_receive = nreceive }

let optimize_program (messages, actors, functions, main) =
  let faster_actors = List.map opt_actor actors in
  let faster_functions = List.map opt_vdecl functions in
  let (main_lit, main_typ) = main in
  let faster_main = match (main_lit) with
        SFunc_Lit(f) -> SFunc_Lit (opt_func_lit f)
      | _ -> raise (Failure "bad main") in
  (messages, faster_actors, faster_functions, (faster_main, main_typ))
