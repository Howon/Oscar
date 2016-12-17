module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)
open Hashtbl
(* module Hashtbl = Hashtbl.Make(String) *)

type scope = {
  loc : string list;
  block_cnt : int;
  if_cnt : int;
  func_cnt : int;
  func : L.llvalue;
}

let print_scope scope =
  let () = print_string "loc: [" in
  let () = List.iter(fun a -> print_string (a ^ " ")) scope.loc in
  let () = print_endline "]" in
  let () = print_endline ("blocks: " ^ string_of_int scope.block_cnt) in
  let () = print_endline ("ifs: " ^ string_of_int scope.if_cnt) in
  print_endline ("funcs: " ^ string_of_int scope.func_cnt)

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

let main_f =
  let main_typ = L.function_type void_t [| |] in
  L.define_function "main" main_typ the_module

(* Define each message so we can build it later *)
let message_decls = StringMap.empty

(* Define each actor so we can build them later *)
let actor_decls = StringMap.empty

let build_name scope s =
  (String.concat "_" scope.loc) ^ "__" ^ s

let rec lookup_helper loc n =
  match List.length loc with
      0 -> None
    | _ -> let name = (String.concat "_" loc) ^ "__" ^ n in
            try Some (Hashtbl.find local_vars name) with
            | Not_found -> lookup_helper (List.tl loc) n

let lookup scope n =
  let fnd = lookup_helper scope.loc n in
  match fnd with
      Some s  -> s
    | None    -> let name = build_name scope n in
                  raise (Failure ("undefined local variable: " ^ name))

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


(* We have to handle Func_Lit slightly separately so we can
 * handle scope within those functiions correctly *)
let rec build_expr builder scope (te : S.t_expr) =
  match te with
    (S.SFunc_Lit(f), typ) ->
      let f_name = (if scope.func_cnt = 0 then "f"
          else "f" ^ (string_of_int scope.func_cnt)) in
      let new_name = build_name scope f_name in
      let func = define_func new_name typ in
      let nscope = {  loc = f_name :: scope.loc ; block_cnt = 0;
                      if_cnt = 0; func_cnt = 0; func = func } in
      let tmp_builder = L.builder_at_end context (L.insertion_block builder) in
      let ret_scope = { scope with func_cnt = scope.func_cnt + 1 } in
      (build_func tmp_builder nscope f, ret_scope)

  | _ -> (build_norm_expr builder scope te, scope)

(* these are all non-funclit expressions
 * We have to break them out because these don't alter scope def,
 * while Func_lits do *)
and build_norm_expr builder scope (te : S.t_expr) =
  match te with
    (S.SInt_Lit(i), typ)          -> L.const_int i32_t i
  | (S.SDouble_Lit(d), typ)       -> L.const_float f_t d
  | (S.SChar_Lit(c), typ)         -> L.const_int i8_t (Char.code c)
  | (S.SString_Lit(s), typ)       -> L.build_global_stringptr s "tmp" builder
  | (S.SBool_Lit(b), typ)         -> L.const_int i1_t (if b then 1 else 0)
  | (S.SUnit_Lit(u), typ)         -> raise (Failure ("no units") )
  | (S.SNoexpr, typ)              -> L.const_int i32_t 0
  | (S.SId(s), typ)               ->
      L.build_load (lookup scope s) (build_name scope s) builder
  | (S.SAccess(e1, e2), typ)      -> raise (Failure ("TODO: access") )
  | (S.SList_Lit(t, exprs), typ)  -> raise (Failure ("TODO: lists") )
  | (S.SSet_Lit(t, exprs), typ)   -> raise (Failure ("TODO: sets") )
  | (S.SMap_Lit(kt, vt, kvs), typ)-> raise (Failure ("TODO: maps") )
  | (S.SActor_Lit(n, exprs), typ) -> raise (Failure ("TODO: actors") )
  | (S.SPool_Lit(n, exprs, c), typ) -> raise (Failure ("TODO: pools") )
  | (S.SMessage_Lit(n, exprs), typ)-> raise (Failure ("TODO: messages") )
  | (S.SBinop(e1, op, e2), typ)   ->
    let name = build_name scope "tmp" in
    let e1' = build_norm_expr builder scope e1
    and e2' = build_norm_expr builder scope e2 in
      (ll_binop op (snd e1)) e1' e2' name builder

  | (S.SUop(op, e), typ) ->
    let name = build_name scope "tmp" in
    let e' = build_norm_expr builder scope e in
      (match op with
          A.Neg -> (match typ with
              A.Int_t -> L.build_neg
            | A.Double_t -> L.build_fneg
            | _ -> raise (Failure ("bad unop") ))
        | A.Not -> L.build_not
      ) e' name builder
  | (S.SFuncCall("Println", t_el), typ) -> build_print_call scope t_el builder
  | (S.SFuncCall(f, act), typ) ->
      let fptr = lookup scope f in
      let fdef = L.build_load fptr (build_name scope "tmpfunc") builder in
      let actuals =
        List.rev (List.map (build_norm_expr builder scope) (List.rev act)) in
      let result = (
        match typ with
            A.Unit_t -> ""
          | _ -> f ^ "_result"
        ) in
      L.build_call fdef (Array.of_list actuals) result builder
  | _ -> raise (Failure ("not an expr"))

and define_func name typ =
  let (typs, rt) = match typ with
      A.Func_t(ts, r) -> (ts, r)
    | _               -> ([], A.Unit_t) in

  let ftype = ltype_of_func typs rt in
  L.define_function name ftype the_module

and build_func builder scope f =
  let () = L.position_at_end (L.entry_block scope.func) builder in

  let new_formals =
    List.map (fun (s,t) -> (build_name scope s, t)) f.S.sf_formals in

  (* add formals to the stack *)
  let () = ignore(List.map2 (add_local builder) new_formals
                    (Array.to_list (L.params scope.func))) in
  (* build the body *)
  let () = ignore(build_stmt builder scope f.S.sf_body) in

  (* add return none if function ends without one *)
  let () = match f.S.sf_return_t with
      A.Unit_t -> ignore (add_terminal builder L.build_ret_void)
    | _ -> () in
  scope.func

(* Takes a list of expressions and builds the correct print call *)
(* t_el is list of t_exprs *)
and build_print_call scope t_el builder =

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
  let params = List.map (build_norm_expr builder scope) pexprs_l in
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

and build_if builder scope predicate then_stmt else_stmt =
  let clean_scope scope name =
    let new_name = (
      if scope.if_cnt = 0 then
        name
      else
        (name ^ (string_of_int scope.if_cnt))
    ) in
    { scope with loc = new_name :: scope.loc ;
                block_cnt = 0; if_cnt = 0; func_cnt = 0 }
  in

  let nscope = clean_scope scope "if" in

  let bool_val = build_norm_expr builder scope predicate in

  let entry_bb = L.insertion_block builder in
  let merge_bb = L.append_block context "finishif" nscope.func in

  (* build then block *)
  let then_bb = L.append_block context "then" nscope.func in
  let () = L.position_at_end then_bb builder in

  let () =  ignore(build_stmt builder nscope then_stmt) in
  let t_mrg = add_terminal builder (L.build_br merge_bb) in

  let nscope = clean_scope scope "else" in

  (* build else block *)
  let else_bb = L.append_block context "else" nscope.func in
  let () = L.position_at_end else_bb builder in
  let () =  ignore(build_stmt builder nscope else_stmt) in
  let e_mrg = add_terminal builder (L.build_br merge_bb) in


  (* wherever our entry block was, go back there and cond branch *)
  let () = L.position_at_end entry_bb builder in
  let () = ignore(L.build_cond_br bool_val then_bb else_bb builder) in

  (* if we need to resolve to a merge, keep it and move builder,
   * otherwise, delete the merge block and go back to caller *)

  let () =
    (if (t_mrg || e_mrg) then
      L.position_at_end merge_bb builder
    else
      let () = (L.delete_block merge_bb) in
      L.position_at_end entry_bb builder
    ) in
  {scope with if_cnt = scope.if_cnt + 1}


(* variable declaration, handling functions *)
and build_vdecl builder scope vd =
  let { S.sv_init = init; S.sv_name = name; S.sv_type = typ } = vd in
  (*let tmp_builder = L.builder_at_end context (L.insertion_block builder) in
  let init_val = build_expr tmp_builder scope init in*)
  let (init_val, nscope) = build_expr builder scope init in
  let new_name = build_name scope name in
  let () = add_local builder (new_name, typ) init_val in
  nscope

and build_block builder scope stmts =
  let nscope =
    if scope.block_cnt = 0 then
      { scope with block_cnt = 1 }
    else
      let new_name = "b" ^ (string_of_int scope.block_cnt) in
      {scope with loc = new_name :: scope.loc; block_cnt = 0;
          if_cnt = 0; func_cnt = 0}
  in


  (* TODO gotta fold left *)
  let () = ignore ( List.map (build_stmt builder nscope) stmts) in
  {scope with block_cnt = scope.block_cnt + 1}

(* Build the code for the given statement; return the new scope *)
and build_stmt builder scope stmt =
  (match stmt with
      S.SExpr(se, typ) ->
        let () = ignore(build_expr builder scope (se, typ)) in
        scope
    | S.SBlock(sl) -> build_block builder scope sl
    | S.SReturn(se, typ) -> (match typ with
            A.Unit_t ->
              let () = ignore(L.build_ret_void builder) in
              scope
          | _ ->
              let (expr, nscope) = build_expr builder scope (se, typ) in
              let () = ignore(L.build_ret expr builder) in
              nscope)
    | S.SVdecl(sval_decl) -> build_vdecl builder scope sval_decl
    | S.SMutdecl(smvar_decl) -> raise ( Failure ("TODO: mutdecl") )
    | S.SIf (predicate, then_stmt, else_stmt) ->
        build_if builder scope predicate then_stmt else_stmt
    | S.SActor_send(e1, e2) -> raise ( Failure ("TODO: act_send") )
    | S.SPool_send(e1, e2) -> raise ( Failure ("TODO: pool_broadcast") ))

let translate (messages, actors, functions, main) =
  let scope = { loc = ["main"]; block_cnt = 0;
                if_cnt = 0; func_cnt = 0; func = main_f } in

  (* build out all other functions *)
  let builder = L.builder_at_end context (L.entry_block main_f) in
  let nscope = List.fold_left (fun nscope f ->
      (build_vdecl builder nscope f)) scope functions
  in
  let () = print_endline "making main!" in
  (* build out actual main function *)
  let () = L.position_at_end (L.entry_block main_f) builder in
  let () = (match (fst main) with
      S.SFunc_Lit(f) -> ignore(build_func builder nscope f)
    | _              -> () ) in
  the_module
