{ open Parser }

let digit = ['0' - '9']
let double = ('-'?)((digit+'.'digit*) | ('.'digit+))
let chr = ['a'-'z' 'A'-'Z' '_' '-' ' ' '\n' '\r' '\t' '\"' '\''] | digit

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

  (* comma, dot, semi *)
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
  | "for"                 { FLOW_FOR }
  | "while"               { FLOW_WHILE }
  | "to"                  { LOOP_TO }
  | "by"                  { LOOP_BY }
  | "<-"                  { LOOP_FROM }

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

  (* non-primitive types *)
  | "string"              { TYPE_STR }
  | "list"                { TYPE_LIST }
  | "set"                 { TYPE_SET }
  | "map"                 { TYPE_MAP }
  | "message"             { TYPE_MESSAGE }
  | "actor"               { TYPE_ACTOR }
  | "pool"                { TYPE_POOL }
  | "def"                 { TYPE_FUNC }
  | "lambda"              { TYPE_LAMBDA }

  (* literals *)
  | digit+ as lxm { INT_LIT(int_of_string lxm) }
  | double as lxm { DOUBLE_LIT(float_of_string lxm)}
  | '\"' ([^'\"']* as lxm) '\"' { STRING_LIT(lxm) }
  | '\'' (chr as lxm) '\'' { CHAR_LIT(lxm) }
  | "true" | "false" as lxm { BOOL_LIT(bool_of_string lxm) }
  (* identifiers *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as lxm { ID(lxm) }
  | eof { EOF }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }
