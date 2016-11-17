(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (messages, actors, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Oscar"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
    | A.Unit_t -> void_t in

  (* Declare print(), which the print built-in function will call *)
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "print" print_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func =
      let name = func.A.name
      and formal_types =
  Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) func.A.formals)
      in let ftype = L.function_type (ltype_of_typ func.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body func =
    let (the_function, _) = StringMap.find func.A.name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.String_t(s) -> L.build_global_stringptr s "tmp" builder
      | A.Call ("print", [e]) ->
              L.build_call print_func [| (expr builder e) |] "print" builder
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
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block func.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match func.A.typ with
      | A.Unit_t -> L.build_ret_void)
  in

  List.iter build_function_body functions;
  the_module
