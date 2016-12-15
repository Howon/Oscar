{
  open Parser

  let make_char c =
    let ec = Char.escaped c in
    let s = Scanf.unescaped (match String.length ec with
        1 -> "\\" ^ ec
      | _ -> ec) in
    String.get s 0

  let fail c = raise (Failure ("scanning failed on char " ^ (Char.escaped c)))
}

let digit = ['0' - '9']
let double = ('-'?)((digit+'.'digit*) | ('.'digit+))
let simp_chr = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let esc_chr = ['t' 'n' 'r' '\'' '\"' '\\']
let all_chr = simp_chr | esc_chr

rule token = parse
    [' ' '\t' '\r' '\n']  { token lexbuf }
  | "/*"                  { comment lexbuf }

  (* braces/parens/brackets *)
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | '<'                   { LANGLE }
  | '>'                   { RANGLE }

  | '='                   { ASSIGN }
  | "->"                  { ARROW }

  (* math *)
  | '+'                   { ARITH_PLUS }
  | '-'                   { ARITH_MINUS }
  | '*'                   { ARITH_TIMES }
  | '/'                   { ARITH_DIVIDE }
  | '%'                   { ARITH_MOD }

  (* comma, semi *)
  | ','                   { PUNC_COMMA }
  | ';'                   { PUNC_SEMI }

  (* logic *)
  | "=="                  { LOGIC_EQ }
  | "!="                  { LOGIC_NEQ }
  | "<="                  { LOGIC_LEQ }
  | ">="                  { LOGIC_GEQ }
  | "&&"                  { LOGIC_AND }
  | "||"                  { LOGIC_OR }
  | "true"                { LOGIC_TRUE }
  | "false"               { LOGIC_FALSE }
  | '!'                   { LOGIC_NOT }

  (* bit math *)
  | '&'                   { BITWISE_AND }
  | '|'                   { BITWISE_OR }
  | '^'                   { BITWISE_XOR }
  | ">>"                  { BITWISE_RIGHT }
  | "<<"                  { BITWISE_LEFT }

  (* functions *)
  | ':'                   { FUNC_ARG_TYPE}
  | "=>"                  { FUNC_RET_TYPE }
  | "return"              { RETURN }

  (* flow control *)
  | "if"                  { FLOW_IF }
  | "else"                { FLOW_ELSE }

  (* actors *)
  | "spawn"               { ACT_SPAWN }
  | "receive"             { ACT_RECEIVE }
  | "|>"                  { ACT_SEND }
  | "|>>"                 { ACT_BROADCAST }

  (* mutability for actors *)
  | "mut"                 { MUTABLE }

  (* primitive types *)
  | "int"                 { TYPE_INT }
  | "double"              { TYPE_DOUBLE }
  | "char"		            { TYPE_CHAR }
  | "bool"                { TYPE_BOOL }
  | "unit"                { TYPE_UNIT }

  (* function declaration *)
  | "def"                 { FUNC_DECL }

  (* non-primitive types *)
  | "string"              { TYPE_STR }
  | "list"                { TYPE_LIST }
  | "set"                 { TYPE_SET }
  | "map"                 { TYPE_MAP }
  | "message"             { TYPE_MESSAGE }
  | "actor"               { TYPE_ACTOR }
  | "pool"                { TYPE_POOL }
  | "func"                { TYPE_FUNC }

  (* literals *)
  | digit+ as lxm { INT_LIT(int_of_string lxm) }
  | double as lxm { DOUBLE_LIT(float_of_string lxm)}
  | '\"' (all_chr* as lxm) '\"' { STRING_LIT(lxm) }
  | '\'' (simp_chr as lxm) '\'' { CHAR_LIT(lxm) }
  | "'\\" (esc_chr as ec) "'" { CHAR_LIT(make_char ec) }
  | "true" | "false" as lxm { BOOL_LIT(bool_of_string lxm) }
  (* identifiers *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as bad { fail bad }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }
