{ open Parser }

let digit = ['0' - '9'];
let double = ('-'?)((digit+'.'digit*) | ('.'digit+));
let chr = '\'' (('a'-'z'|'A'-'Z')|'\\'['\\' '*' 'n' 'r' 't' '"' '''];

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
  | '<'                   { LANGLE_BRACKET }
  | '>'                   { RANGLE_BRACKET }

  | '='                   { ASSIGN }

  (* math *)
  | '+'                   { ARITH_PLUS }
  | '-'                   { ARITH_MINUS }
  | '*'                   { ARITH_TIMES }
  | '/'                   { ARITH_DIVIDE }
  | '%'                   { ARITH_MOD }

  (* comma, dot, semi *)
  | '.'                   { PUNC_DOT }
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

  (* bit math *)
  | '&'                   { BITWISE_AND }
  | '|'                   { BITWISE_OR }
  | '^'                   { BITWISE_XOR }
  | '~'                   { BITWISE_NOT }
  | ">>"                  { BITSHIFT_RIGHT }
  | "<<"                  { BITSHIFT_LEFT }

  (* functions *)
  | ':'                   { FUNC_ARG_TYPE}
  | "=>"                  { FUNC_TYPE }
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
  | "sender"              { ACT_SENDER }
  | "die"                 { ACT_DIE }
  | "spawn"               { ACT_SPAWN }
  | "receive"             { ACT_RECEIVE }
  | "|>"                  { ACT_SEND }
  | "|>>"                 { ACT_BROADCAST }

  (* mutability for actors *)
  | "mut"                 { MUTABLE }

  (* primitive types *)
  | "int"                 { TYPE_INT }
  | "double"              { TYPE_DOUBLE }
  | "char"		          { TYPE_CHAR }
  | "bool"                { TYPE_BOOL }
  | "unit"                { TYPE_UNIT }

  (* non-primitive types *)
  | "string"              { TYPE_STR }
  | "maybe"               { TYPE_MAYBE }
  | "none"                { TYPE_NONE }
  | "some"                { TYPE_OPTION_SOME }
  | "list"                { TYPE_LIST }
  | "set"                 { TYPE_SET }
  | "map"                 { TYPE_MAP }
  | "tup"                 { TYPE_TUPLE }
  | "message"             { TYPE_MESSAGE }
  | "actor"               { TYPE_ACTOR }
  | "pool"                { TYPE_POOL }
  | "def"                 { TYPE_DEF }

  (* literals *)
  | digit+ as lxm { INT_LIT(int_of_string lxm) }
  | double as lxm { DOUBLE_LIT(float_of_string lxm)}
  | '\"' ([^'\"']* as lxm) '\"' { STRING_LIT(lxm) }
  | chr as lxm '\'' { CHAR_LIT(lxm) }
  | "true"|"false" as lxm { BOOL_LIT(bool_of_string lxm) }
  (* identifiers *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }

and comment = parse
  | "*/" { token lexbuf }
  | _    { token lexbuf }
