module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)
open Hashtbl
(* module Hashtbl = Hashtbl.Make(String) *)

let local_vars = Hashtbl.create 50

let translate (messages, actors, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Oscar"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and f_t    = L.double_type context
  and void_t = L.void_type context in

  let ltype_of_typ (typ : A.types) = match typ with
      A.Int_t     -> i32_t
    | A.Char_t    -> i8_t
    | A.Bool_t    -> i1_t
    | A.Unit_t    -> void_t
    | A.Double_t  -> f_t
    | A.String_t  -> L.pointer_type i8_t
  in

  (* Declare print(), which the print built-in function will call *)
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func =
      let name = func.S.sf_name
      and formal_types =
         Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) func.S.sf_formals)
      in
      let ftype = L.function_type (ltype_of_typ func.S.sf_return_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body func =

    (* clear hashtable *)
    ignore(Hashtbl.clear local_vars);

    (* params to types *)
    (*
    let rec map_param_to_type = function
        S.SInt_Lit(_)      -> A.Int_t
      | S.SBool_Lit(_)     -> A.Bool_t
      | S.SDouble_Lit(_)   -> A.Double_t
      | S.SChar_Lit(_)     -> A.Char_t
      | S.SString_Lit(_)   -> A.String_t
      | S.SBinop(e1, _, _) -> map_param_to_type e1
                                (* temp fix, grabs type of left arg *)
      | S.SUop(_, e)       -> map_param_to_type e
      | S.SFuncCall(_, _)      -> A.Int_t
      (* todo: this assumes type is int; should grab type from semantic analysis *)
      | S.SId(_)           -> A.Int_t
    in
    *) 

    let (the_function, _) = StringMap.find func.S.sf_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Add a local varible to the Hashtbl local_vars:
        Allocate on the stack, initialize its value, 
        and add to Hashtbl local_vars *)
    let add_local (n, t) p = 
      let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore(L.build_store p local builder);
        Hashtbl.add local_vars n local;
        L.set_value_name n p;
    in 
    (* add all the formals for the function to the Hashtbl local_vars *)
    ignore(List.map2 add_local func.S.sf_formals (Array.to_list (L.params the_function)));


    (* Return the value for a variable or formal argument *)
    let lookup n =
      try Hashtbl.find local_vars n with
          | Not_found -> raise (Failure ("undefined local variable: " ^ n))
    in

    (* Construct code for an expression; return its value *)
    (* takes a t_expr, which is (sexpr, types) *)
    let rec t_expr builder (te : S.t_expr) = 
      match te with
        (S.SInt_Lit(i), typ)          -> L.const_int i32_t i
      | (S.SBool_Lit(b), typ)         -> L.const_int i1_t (if b then 1 else 0)
      | (S.SDouble_Lit(d), typ)       -> L.const_float f_t d
      | (S.SChar_Lit(c), typ)         -> L.const_int i8_t (Char.code c)
      | (S.SString_Lit(s), typ)       -> L.build_global_stringptr s "tmp" builder
      | (S.SNoexpr, typ)              -> L.const_int i32_t 0
      | (S.SId(s), typ)               -> L.build_load (lookup s) s builder
      | (S.SBinop(e1, op, e2), typ)   ->
        let e1' = t_expr builder e1 
        and e2' = t_expr builder e2 in
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
          ) e1' e2' "tmp" builder
      | (S.SUop(op, e), typ) ->
        let e' = t_expr builder e in
          (match op with
              A.Neg -> (match typ with
                  A.Int_t -> L.const_neg
                | A.Double_t -> L.const_fneg)
          ) e'
      | (S.SFuncCall("Println", t_el), typ) -> build_print_call t_el builder
      | (S.SFuncCall(f, act), typ) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (t_expr builder) (List.rev act)) in
          let result = (
            match fdecl.S.sf_return_t with
                A.Unit_t -> ""
              | _ -> f ^ "_result"
            ) in
          L.build_call fdef (Array.of_list actuals) result builder

    (* Takes a list of expressions and builds the correct print call *)
    (* t_el is list of t_exprs *)
    and build_print_call t_el builder =

      (* special expression matcher that turns bools into strings,
          but leaves everything else normal *)
      let bool_to_string (te : S.t_expr) = match te with
          (S.SBool_Lit(b), typ) -> (S.SString_Lit(if b then "true" else "false"), A.String_t)
        | (se, typ) -> (se, typ)
      in

      (* type to string used to print *)
      let map_type_to_string = function
          A.Int_t           -> "%d"
        | A.Bool_t          -> "%d" (* todo bools print as ints *)
        | A.Double_t        -> "%f"
        | A.Char_t          -> "%c"
        | A.String_t        -> "%s"

      in

      (* get the things to print *)
      let new_t_el = List.map bool_to_string t_el in
      let params = List.map (t_expr builder) new_t_el in
      (* get their types *)
      let param_types = List.map snd new_t_el in

      let const_str = List.fold_left
                        (fun s t -> s ^ map_type_to_string t) "" param_types
      in
      (* default add newline *)
      let fmt_str = L.build_global_stringptr
                        (const_str ^ "\n") "tmp" builder in
      L.build_call print_func
                        (Array.of_list (fmt_str :: params)) "printf" builder
      in

    (* Invoke "f builder" if the current block doesn't already
    have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        S.SExpr(se, typ) -> ignore (t_expr builder (se, typ)); builder
      | S.SBlock(sl) -> List.fold_left stmt builder sl
      | S.SReturn(se, typ) -> ignore(
          match func.S.sf_return_t with
              A.Unit_t -> L.build_ret_void builder
            | _ -> L.build_ret (t_expr builder (se, typ)) builder
          ); builder
      | S.SVdecl(sval_decl) ->
          let init_val = t_expr builder sval_decl.sv_init in 
            let tup = (sval_decl.sv_name, sval_decl.sv_type) in
              add_local tup init_val;
            builder;
      | S.SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = t_expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
            add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
            (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
            add_terminal (stmt (L.builder_at_end context else_bb) else_stmt) 
            (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder func.S.sf_body in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (
      match func.S.sf_return_t with
          A.Unit_t -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
    )
  in

  List.iter build_function_body functions;
  the_module
