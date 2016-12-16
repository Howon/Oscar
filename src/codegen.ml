module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)
open Hashtbl
(* module Hashtbl = Hashtbl.Make(String) *)

let local_vars = Hashtbl.create 50

let context = L.global_context ()
let the_module = L.create_module context "Oscar"
and i32_t  = L.i32_type context
and i8_t   = L.i8_type context
and i1_t   = L.i1_type context
and f_t    = L.double_type context
and void_t = L.void_type context

let rec ltype_of_typ (typ : A.types) = match typ with
    A.Int_t     -> i32_t
  | A.Char_t    -> i8_t
  | A.Bool_t    -> i1_t
  | A.Unit_t    -> void_t
  | A.Double_t  -> f_t
  | A.String_t  -> L.pointer_type i8_t
  | A.Func_t (typs, rt) -> L.pointer_type (ltype_of_func typs rt)
  | _           -> raise (Failure ("TODO implement types") )

and ltype_of_func typs rt =
  let formal_types =
    Array.of_list (List.map (fun t -> ltype_of_typ t) typs)
  and ret_typ = ltype_of_typ rt in
  L.function_type ret_typ formal_types


(* Declare print(), which the print built-in function will call *)
let print_func =
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  L.declare_function "printf" print_t the_module

let main =
  let main_typ = L.function_type void_t [| |] in
  L.define_function "main" main_typ the_module

(* Define each message so we can build it later *)
let message_decls = StringMap.empty

(* Define each actor so we can build them later *)
let actor_decls = StringMap.empty

let lookup n =
  try Hashtbl.find local_vars n with
      | Not_found -> raise (Failure ("undefined local variable: " ^ n))


let add_local builder (n, t) p =
  let local = L.build_alloca (ltype_of_typ t) n builder in
    let () = ignore(L.build_store p local builder) in
    let () = ignore(Hashtbl.add local_vars n local) in
    ignore(L.set_value_name n p)


let ll_binop op typ =
    (match typ with
      A.Int_t | A.Char_t | A.Bool_t ->
        (match op with
            A.Add         -> L.build_add
          | A.Sub         -> L.build_sub
          | A.Mult        -> L.build_mul
          | A.Div         -> L.build_sdiv
          | A.Mod         -> L.build_srem
          | A.Equal       -> L.build_icmp L.Icmp.Eq
          | A.Neq         -> L.build_icmp L.Icmp.Ne
          | A.Less        -> L.build_icmp L.Icmp.Slt
          | A.Leq         -> L.build_icmp L.Icmp.Sle
          | A.Greater     -> L.build_icmp L.Icmp.Sgt
          | A.Geq         -> L.build_icmp L.Icmp.Sge
          | A.And         -> L.build_and
          | A.Or          -> L.build_or
          | A.Bit_And     -> L.build_and
          | A.Bit_Or      -> L.build_or
          | A.Bit_Xor     -> L.build_xor
          | A.Bit_RShift  -> L.build_lshr
          | A.Bit_LShift  -> L.build_shl
          | _             -> raise (Failure ("bad int/char/bool binop") )
        )
    (* TODO: make void ops work *)
    | A.Unit_t                      ->
        (match op with
          | A.Equal       -> L.build_icmp L.Icmp.Eq
          | A.Neq         -> L.build_icmp L.Icmp.Ne
          | _             -> raise (Failure ("bad unit binop") )
        )
    | A.Double_t                    ->
        (match op with
            A.Add         -> L.build_fadd
          | A.Sub         -> L.build_fsub
          | A.Mult        -> L.build_fmul
          | A.Div         -> L.build_fdiv
          | A.Equal       -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq         -> L.build_fcmp L.Fcmp.One
          | A.Less        -> L.build_fcmp L.Fcmp.Olt
          | A.Leq         -> L.build_fcmp L.Fcmp.Ole
          | A.Greater     -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq         -> L.build_fcmp L.Fcmp.Oge
          | _             -> raise (Failure ("bad double binop") )
        )
    (* TODO: make string ops work *)
    | A.String_t                    ->
        (match op with
            A.Add         -> L.build_add
          | A.Equal       -> L.build_icmp L.Icmp.Eq
          | A.Neq         -> L.build_icmp L.Icmp.Ne
          | _             -> raise (Failure ("bad string binop") )
        )

    | A.List_t(t)         -> raise (Failure ("binop TODO") )
    | A.Set_t(s)          -> raise (Failure ("binop TODO") )
    | A.Map_t(kt, vt)     -> raise (Failure ("binop TODO") )
    | _                   -> raise (Failure ("bad binop" ) )
  )



(* Invoke "f builder" if the current block doesn't already
have a terminal (e.g., a branch). *)
let add_terminal builder f =
  match L.block_terminator (L.insertion_block builder) with
      Some _ -> false
    | None -> (let () = ignore (f builder) in true)

let rec build_expr builder (te : S.t_expr) =
  match te with
    (S.SInt_Lit(i), typ)          -> L.const_int i32_t i
  | (S.SDouble_Lit(d), typ)       -> L.const_float f_t d
  | (S.SChar_Lit(c), typ)         -> L.const_int i8_t (Char.code c)
  | (S.SString_Lit(s), typ)       -> L.build_global_stringptr s "tmp" builder
  | (S.SBool_Lit(b), typ)         -> L.const_int i1_t (if b then 1 else 0)
  | (S.SUnit_Lit(u), typ)         -> raise (Failure ("no units") )
  | (S.SNoexpr, typ)              -> L.const_int i32_t 0
  | (S.SId(s), typ)               -> L.build_load (lookup s) s builder
  | (S.SAccess(e1, e2), typ)      -> raise (Failure ("TODO: access") )
  | (S.SFunc_Lit(f), typ)      ->
      let func = define_func "tmp" typ in
      build_func builder func f
  | (S.SList_Lit(t, exprs), typ)  -> raise (Failure ("TODO: lists") )
  | (S.SSet_Lit(t, exprs), typ)  -> raise (Failure ("TODO: sets") )
  | (S.SMap_Lit(kt, vt, kvs), typ)-> raise (Failure ("TODO: maps") )
  | (S.SActor_Lit(n, exprs), typ) -> raise (Failure ("TODO: actors") )
  | (S.SPool_Lit(n, exprs, c), typ) -> raise (Failure ("TODO: pools") )
  | (S.SMessage_Lit(n, exprs), typ)-> raise (Failure ("TODO: messages") )
  | (S.SBinop(e1, op, e2), typ)   ->
    let e1' = build_expr builder e1
    and e2' = build_expr builder e2 in
      (ll_binop op (snd e1)) e1' e2' "tmp" builder

  | (S.SUop(op, e), typ) ->
    let e' = build_expr builder e in
      (match op with
          A.Neg -> (match typ with
              A.Int_t -> L.build_neg
            | A.Double_t -> L.build_fneg
            | _ -> raise (Failure ("bad unop") ))
        | A.Not -> L.build_not
      ) e' "tmp" builder
  | (S.SFuncCall("Println", t_el), typ) -> build_print_call t_el builder
  | (S.SFuncCall(f, act), typ) ->
      let fdef = lookup f in
      let actuals = List.rev (List.map (build_expr builder) (List.rev act)) in
      let result = (
        match typ with
            A.Unit_t -> ""
          | _ -> f ^ "_result"
        ) in
      L.build_call fdef (Array.of_list actuals) result builder

and define_func name typ =
  let (typs, rt) = match typ with
      A.Func_t(ts, r) -> (ts, r)
    | _               -> ([], A.Unit_t) in

  let ftype = ltype_of_func typs rt in
  L.define_function name ftype the_module

and build_func builder func f =
  let () = ignore(Hashtbl.clear local_vars) in

  let () = L.position_at_end (L.entry_block func) builder in

  (* add formals to the stack *)
  let () = ignore(List.map2 (add_local builder) f.S.sf_formals
                    (Array.to_list (L.params func))) in
  (* build the body *)
  let () = build_stmt builder func f.S.sf_body in

  (* add return none if function ends without one *)
  let () = match f.S.sf_return_t with
      A.Unit_t -> ignore (add_terminal builder L.build_ret_void)
    | _ -> () in
  func

(* Takes a list of expressions and builds the correct print call *)
(* t_el is list of t_exprs *)
and build_print_call t_el builder =

  (* make a function that handles anything with bool_t as an if *)
  let to_print_texpr texpr = match (snd texpr) with
      A.Bool_t  ->
        (S.SFuncCall("AsString", [texpr]), A.String_t)
    | _     -> texpr
  in

  (* type to string used to print *)
  let map_type_to_string = function
      A.Int_t           -> "%d"
    | A.Bool_t          -> "%d"
    | A.Double_t        -> "%f"
    | A.Char_t          -> "%c"
    | A.String_t        -> "%s"
    | _                 -> raise ( Failure("TODO: printing types"))

  in

  (* get the things to print *)
  let pexprs_l = List.map to_print_texpr t_el in
  let params = List.map (build_expr builder) pexprs_l in
  (* get their types *)
  let param_types = List.map snd pexprs_l in

  let const_str = List.fold_left
                    (fun s t -> s ^ map_type_to_string t) "" param_types
  in
  (* default add newline *)
  let fmt_str = L.build_global_stringptr
                    (const_str ^ "\n") "tmp" builder in
  L.build_call print_func
                    (Array.of_list (fmt_str :: params)) "printf" builder

and build_if builder the_function predicate then_stmt else_stmt =
  let bool_val = build_expr builder predicate in

  let entry_bb = L.insertion_block builder in
  let merge_bb = L.append_block context "finishif" the_function in

  (* build then block *)
  let then_bb = L.append_block context "then" the_function in
  let () = L.position_at_end then_bb builder in
  let () =  build_stmt builder the_function then_stmt in
  let t_mrg = add_terminal builder (L.build_br merge_bb) in

  (* build else block *)
  let else_bb = L.append_block context "else" the_function in
  let () = L.position_at_end else_bb builder in
  let () =  build_stmt builder the_function else_stmt in
  let e_mrg = add_terminal builder (L.build_br merge_bb) in


  (* wherever our entry block was, go back there and cond branch *)
  let () = L.position_at_end entry_bb builder in
  let () = ignore(L.build_cond_br bool_val then_bb else_bb builder) in

  (* if we need to resolve to a merge, keep it and move builder,
   * otherwise, delete the merge block and go back to caller *)

  if (t_mrg || e_mrg) then
    L.position_at_end merge_bb builder
  else
    let () = (L.delete_block merge_bb) in
    L.position_at_end entry_bb builder

(* variable declaration, handling functions *)
and build_vdecl builder vd =
  let { S.sv_init = init; S.sv_name = name; S.sv_type = typ } = vd in
  match name with
      "main"  -> (match (fst init) with
                    S.SFunc_Lit(f) -> ignore(build_func builder main f)
                  | _           -> ())
    | _       ->
        let tmp_builder = L.builder_at_end context (L.insertion_block builder) in
        let init_val = build_expr tmp_builder init in
        add_local builder (name, typ) init_val

  (* Build the code for the given statement; return the builder for
     the statement's successor *)
and build_stmt builder func stmt =
  match stmt with
      S.SExpr(se, typ) -> ignore (build_expr builder (se, typ))
    | S.SBlock(sl) ->
        ignore(List.map ( build_stmt builder func) sl)
    | S.SReturn(se, typ) -> ignore(
        match typ with
            A.Unit_t -> L.build_ret_void builder
          | _ -> L.build_ret (build_expr builder (se, typ)) builder
        )
    | S.SVdecl(sval_decl) -> build_vdecl builder sval_decl
    | S.SMutdecl(smvar_decl) -> raise ( Failure ("TODO: mutdecl") )
    | S.SIf (predicate, then_stmt, else_stmt) ->
        build_if builder func predicate then_stmt else_stmt
    | S.SActor_send(e1, e2) -> raise ( Failure ("TODO: act_send") )
    | S.SPool_send(e1, e2) -> raise ( Failure ("TODO: pool_broadcast") )

let translate (messages, actors, functions) =
  let builder = L.builder_at_end context (L.entry_block main) in
  List.iter (build_vdecl builder) functions;
  the_module
