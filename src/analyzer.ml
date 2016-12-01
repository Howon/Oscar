open Ast
open Sast
open Exception

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type vsymtab = {
    vparent      : vsymtab option;
    vals         : val_decl list;
}

type mvsymtab = {
    mvparent     : mvsymtab option;
    mvars        : mvar_decl list;
}

type actor_map = {
    aformals_list     : formal list;
    afunc_list        : sfunc list;
    areceive          : spattern list;
}

type environ = {
    vsymbol_table : vsymtab;
    mvsymbol_table : mvsymtab;
    return_type : types option;
    funcs : sfunc list;
    in_loop : bool
}

let rec find_value_decl (scope : vsymtab) v_name =
    try
        List.find (fun val_decl -> val_decl.v_name = v_name) scope.vals
    with Not_found ->
        match scope.vparent with
          Some(vparent) -> find_value_decl vparent v_name
        | _ -> raise Not_found

let rec find_variable_decl (scope : mvsymtab) mv_name =
    try
        List.find (fun mvar_decl -> mvar_decl.mv_name = mv_name) scope.mvars
    with Not_found ->
        match scope.mvparent with
          Some mvparent -> find_variable_decl mvparent mv_name
        | _ -> raise Not_found

let find_type (vstab : vsymtab) (mvstab : mvsymtab) name =
    try
        let vtype = find_value_decl vstab name in
        try
            let mvtype = find_variable_decl mvstab name in mvtype.mv_type
        with
              Not_found -> vtype.v_type
            | _ ->
                raise Exception.Duplicate_Decl
    with Not_found ->
        try
            let mvtype = find_variable_decl mvstab name in mvtype.mv_type
        with Not_found -> raise Not_found

let string_of_bin_op op =
    match op with
      Add        -> "+"
    | Sub        -> "-"
    | Mult       -> "*"
    | Div        -> "/"
    | Mod        -> "%"
    | Equal      -> "=="
    | Neq        -> "!="
    | Less       -> "<"
    | Leq        -> "<="
    | Greater    -> ">"
    | Geq        -> ">="
    | And        -> "&&"
    | Or         -> "||"
    | Bit_And    -> "&"
    | Bit_Or     -> "|"
    | Bit_Xor    -> "^"
    | Bit_RShift -> ">>"
    | Bit_LShift -> "<<"

let check_binop (e1 : sexpr) (e2 : sexpr) (op : bin_op) (env : environ) =
    match op with
        Add ->
          (match e1, e2 with
              SInt_Lit(_), SInt_Lit(_)  -> Int_t
            | SDouble_Lit(_), SDouble_Lit(_) -> Double_t
            | SString_Lit(_), SString_Lit(_) -> String_t
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | Sub | Mult | Div | Neq | Less | Leq | Greater | Geq ->
          (match e1, e2 with
              SInt_Lit(_), SInt_Lit(_)  -> Int_t
            | SDouble_Lit(_), SDouble_Lit(_) -> Double_t
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                                      " string_of_bin_op op")))
      | Mod | Bit_And | Bit_Or | Bit_Xor | Bit_RShift | Bit_LShift ->
          (match e1, e2 with
              SInt_Lit(_), SInt_Lit(_)  -> Int_t
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                             " string_of_bin_op op")))
      | Equal | Neq ->
          (match e1, e2 with
              SInt_Lit(_), SInt_Lit(_)       -> Int_t
            | SDouble_Lit(_), SDouble_Lit(_) -> Double_t
            | SChar_Lit(_), SChar_Lit(_)     -> Char_t
            | SString_Lit(_), SString_Lit(_) -> String_t
            | SBool_Lit(_), SBool_Lit(_)     -> Bool_t
            | SList_Lit(t1, _), SList_Lit(t2, _) when t1 = t2 -> List_t(t1)
            | SMap_Lit(kt1, vt1, _), SMap_Lit(kt2, vt2, _)
                    when (kt1 = kt2) && (vt1 = vt2) -> Map_t(kt1, vt1)
            | SSet_Lit(t1, _), SSet_Lit(t2, _)
                    when t1 = t2 -> Set_t(t1)
            | STuple_Lit(t1, _), STuple_Lit(t2, _) when t1 = t2 -> Tuple_t(t1)
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
      | And | Or ->
            (match e1, e2 with
              SBool_Lit(_), SBool_Lit(_) -> Bool_t
            | _ -> raise (Invalid_argument ("operand type mismatch: " ^
                            " string_of_bin_op op")))
