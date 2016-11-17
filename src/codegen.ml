module A = Ast
module L = Llvm

module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "Oscar"
let builder = L.builder_at_end context (L.entry_block the_function)

let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func = L.declare_function "printf" printf_t the_module in
let rec codegen_expr = function
	A.Call ("print", [e]) | A.Call ("printb", [e]) ->
		L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder;

the_module
