open Ast

let rec eval_string = function
      Id(x) -> x  (* todo: or Id(x) should return the variable's value from symbol table? *)
    | String_Lit(x) -> x
    (* | Binop(e1, op, e2) ->
        let v1 = eval_string e1 and v2 = eval_string e2 in
        (
          match op with
              Equal -> "foo"
              (* Equal -> v1 = v2 *)
            (* todo: or != instead?? *)
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2
        ) *)
    (* todo: make it right *)
    (* | Assign(x, e) -> symbol_table[x] = (eval_string e) *)

let rec eval_char = function
      Char_Lit(x) -> x
(*     | Binop(e1, op, e2) ->
        let v1 = eval_char e1 and v2 = eval_char e2 in
        (
          match op with
              Equal -> v1 = v2
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2
        ) *)
    (* todo: make it right *)
    (* | Assign(x, e) -> symbol_table[x] = (eval_char e) *)

let rec eval_bool = function
      Bool_Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval_bool e1 and v2 = eval_bool e2 in
        (
          match op with
              Equal -> v1 = v2
            (* todo: or != instead?? *)
            | Neq -> v1 <> v2
            | And -> v1 && v2
            | Or -> v1 || v2
        )
    | Unop(op, e) ->
        let v = eval_bool e in
        (
          match op with
            Not -> not v
        )
    (* todo: make it right *)
    (* | Assign(x, e) -> symbol_table[x] = (eval_bool e) *)

let rec eval_int = function
      Int_Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval_int e1 and v2 = eval_int e2 in
        (
          match op with
              Add -> v1 + v2
            | Sub -> v1 - v2
            | Mult -> v1 * v2
            | Div -> v1 / v2
            | Mod -> v1 mod v2
            (* | Equal -> v1 = v2
            (* todo: or != instead?? *)
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2 *)
            | Bit_And -> v1 land v2
            | Bit_Or -> v1 lor v2
            | Bit_Xor -> v1 lxor v2
            | Bit_RShift -> v1 lsr v2
            | Bit_LShift -> v1 lsl v2
        )
    | Unop(op, e) ->
        let v = eval_int e in
        (
          match op with
              Neg -> -v
        )
    (* todo: make it right *)
    (* | Assign(x, e) -> symbol_table[x] = (eval_int e) *)

let rec eval_double = function
      Double_Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval_double e1 and v2 = eval_double e2 in
        (
          match op with
          (* todo: introduce op. versions (e.g. +., -., etc)? *)
              Add -> v1 +. v2
            | Sub -> v1 -. v2
            | Mult -> v1 *. v2
            | Div -> v1 /. v2
            (* | Equal -> v1 = v2
            (* todo: or != instead?? *)
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2 *)
        )
    | Unop(op, e) ->
        let v = eval_double e in
        (
          match op with
              Neg -> -.v
        )
    (* todo: make it right *)
    (* | Assign(x, e) -> symbol_table[x] = (eval_double e) *)


let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline (string_of_int result)
