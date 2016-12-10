module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (messages, actors, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Oscar"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and f_t    = L.double_type context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int_t     -> i32_t
    | A.Bool_t    -> i1_t
    | A.Unit_t    -> void_t
    | A.Double_t  -> f_t
  in

  (* Declare print(), which the print built-in function will call *)
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func =
      let name = func.A.f_name
      and formal_types =
         Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) func.A.f_formals)
      in
      let ftype = L.function_type (ltype_of_typ func.A.f_return_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body func =

    (* params to types *)
    let rec map_param_to_type = function
        A.Int_Lit(_)      -> A.Int_t
      | A.Bool_Lit(_)     -> A.Bool_t
      | A.Double_Lit(_)   -> A.Double_t
      | A.Char_Lit(_)     -> A.Char_t
      | A.String_Lit(_)   -> A.String_t
      | A.Binop(e1, _, _) -> map_param_to_type e1
                                (* temp fix, grabs type of left arg *)
      | A.Uop(_, e)       -> map_param_to_type e
      | A.FuncCall(_, _)      -> A.Int_t
      (* todo: this assumes type is int; should grab type from semantic analysis *)
      | A.Id(_)           -> A.Int_t
    in

    let (the_function, _) = StringMap.find func.A.f_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (n, t) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in

      (* not adding locals for now, note locals should be in format (type, name)
          as opposed to (name, type) for formals above *)
      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        StringMap.add n local_var m in

        let formals = List.fold_left2 add_formal StringMap.empty func.A.f_formals
          (Array.to_list (L.params the_function))
        in
        (* no adding locals for now, empty list as a substitute *)
      List.fold_left add_local formals []
    in

    (* Return the value for a variable or formal argument *)
    let lookup n =
      try StringMap.find n local_vars with
          | Not_found -> raise (Failure ("undefined local variable: " ^ n))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Int_Lit(i) -> L.const_int i32_t i
      | A.Bool_Lit(b) -> L.const_int i1_t (if b then 1 else 0)
      | A.Double_Lit(d) -> L.const_float f_t d
      | A.Char_Lit(c) -> L.const_int i8_t (Char.code c)
      | A.String_Lit(s) -> L.build_global_stringptr s "tmp" builder
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop(e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Bit_And -> L.build_and
            | A.Bit_Or -> L.build_or
            | A.Bit_Xor -> L.build_xor
            | A.Bit_RShift -> L.build_lshr
            | A.Bit_LShift -> L.build_shl
          ) e1' e2' "tmp" builder
      | A.Uop(op, e) ->
        let e' = expr builder e in
          (match op with
              A.Neg -> (match (map_param_to_type e) with
                  A.Int_t -> L.const_neg
                | A.Double_t -> L.const_fneg)
            | A.Not -> L.const_not
          ) e'
      | A.FuncCall("println", el) -> build_print_call el builder
      | A.FuncCall (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (
            match fdecl.A.f_return_t with
                A.Unit_t -> ""
              | _ -> f ^ "_result"
            ) in
          L.build_call fdef (Array.of_list actuals) result builder

    (* Takes a list of expressions and builds the correct print call *)
    and build_print_call el builder =

      (* special expression matcher that turns bools into strings,
          but leaves everything else normal *)
      let expr_with_bool_string builder = function
          A.Bool_Lit(b) -> expr builder (A.String_Lit(if b then "true" else "false"))
        | e -> expr builder e
      in

      (* type to string used to print *)
      let map_type_to_string = function
          A.Int_t           -> "%d"
        | A.Bool_t          -> "%s"
        | A.Double_t        -> "%f"
        | A.Char_t          -> "%c"
        | A.String_t        -> "%s"

      in

      let params = List.map (expr_with_bool_string builder) el in
      let param_types = List.map map_param_to_type el in

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
        A.Expr e -> ignore (expr builder e); builder
      | A.Block sl -> List.fold_left stmt builder sl
      | A.Return e -> ignore(
          match func.A.f_return_t with
              A.Unit_t -> L.build_ret_void builder
            | _ -> L.build_ret (expr builder e) builder
          ); builder
      | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
      (* todo: ??? *)
      (* | A.Local(t, s, e) ->
          L.build_alloca (ltype_of_typ t) s builder in
          ignore(L.build_store (expr builder e) local builder) *)
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder func.A.f_body in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (
      match func.A.f_return_t with
          A.Unit_t -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
    )
  in

  List.iter build_function_body functions;
  the_module
