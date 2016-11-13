open Ast

let rec eval = function
      Id(x) -> x
    | Int_Lit(x) -> x
    | Double_Lit(x) -> x
    | Char_Lit(x) -> x
    | String_Lit(x) -> x
    | Bool_Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval e1 and v2 = eval e2 in
        (
          match op with
              Add -> v1 + v2
            | Sub -> v1 - v2
            | Mult -> v1 * v2
            | Div -> v1 / v2
            | Mod -> v1 mod v2
            | Equal -> v1 = v2
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2
            | And -> v1 && v2
            | Or -> v1 || v2
            | Bit_And -> v1 land v2
            | Bit_Or -> v1 lor v2
            | Bit_Xor -> v1 lxor v2
            | Bit_RShift -> v1 lsr v2
            | Bit_LShift -> v1 lsl v2
        )
    | Unop(op, e) ->
        let v = eval e in
        (
          match op with
              Neg -> -v
            | Not -> !v
            (* todo: i couldn't find Bit_Not in OCaml *)
            (* | Bit_Not -> ~v *)
        )
    (* todo: make it right *)
    | Assign(x, e) ->  x ^ " = " ^ eval e
    (* todo: make it right
    | Call(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map eval el) ^ ")"
    *)
    | Noexpr -> ""

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline (string_of_int result)
