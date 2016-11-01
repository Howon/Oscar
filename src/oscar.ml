open Ast

let rec eval = function 
      Id(s) -> s
    | Int_Lit(x) -> x
    | Double_Lit(x) -> x
    | Char_Lit(x) -> x
    | String_Lit(x) -> x
    | Bool_Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval e1 and v2 = eval e2 in
        match op with
              Add -> v1 + v2
            | Sub -> v1 - v2
            | Mult -> v1 * v2
            | Div -> v1 / v2
            | Mod -> v1 % v2
            | Equal -> v1 = v2
            | Neq -> v1 <> v2
            | Less -> v1 < v2
            | Leq -> v1 <= v2
            | Greater -> v1 > v2
            | Geq -> v1 >= v2
            | And -> v1 && v2
            | Or -> v1 || v2
    | Unop(op, e) ->
        let v = eval e in
        match op with
            Neg -> -v
    | Asn(x, e) ->  x ^ " = " ^ string_of_expr e
    (* todo: make it right
    | Call(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    *)
    | Noexpr -> ""

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline (string_of_int result)
