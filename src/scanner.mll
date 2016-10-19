{ open Parser }

rule token = parse
    | [' ' '\t' '\r' '\n']  { token lexbuf }
    | "/*"                  { comment lexbuf }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '{'                   { LBRACE }
    | '}'                   { RBRACE }
    | '['                   { LBRACKET }
    | ']'                   { RBRACKET }
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIVIDE }
    | ','                   { COMMA }
    | '='                   { ASIGN }
	| '%'					{ MODULO }
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | '<'                   { LT }
    | "<="                  { LEQ }
    | '>'                   { GT }
    | ">="                  { GEQ }
    | "&&"                  { AND }
    | "||"                  { OR }
    | '!'                   { NOT }
	| '&'					{ BITWISE_AND }
	| '|'					{ BITWISE_OR }
	| '~'					{ BITWISE_NOT }
	| '.'					{ APPLY_METHOD }
	| ':'					{ FUNCTION_TYPE }
    | "->"                  { ARROW }
    | "=>"                  { FUNCTION }
    | "|>"                  { SEND }
    | "|>>"                 { BROADCAST }
    | "true"                { TRUE }
    | "false"               { FALSE }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "for"                 { FOR }
    | "to"                  { LOOP_TO }
    | "by"                  { LOOP_INC }
    | "return"              { RETURN }
    | "message"             { MESSAGE }
    | "actor"               { ACTOR }
    | "sender"              { SENDER }
    | "pool"                { POOL }
    | "die"                 { DIE }
    | "spawn"               { SPAWN }
    | "MAYBE"               { OPTION_MAYBE }
    | "NONE"                { OPTION_NONE }
    | "SOME"                { OPTION_SOME }
    | "int"                 { INT }
    | "double"              { DOUBLE }
    | "bool"                { BOOL }
    | "string"              { STR }
    | "list"                { LIST }
	| "mut"					{ MUTABLE }
	| "char"				{ CHAR }
	| "def"					{ DEF }
	| "tup"					{ TUPLE }
	| "map"					{ MAP }
    | ['0'-'9']+ as lxm { LITERAL(int_of_string lit) }
	| double as lxm { DOUBLE_LITERAL(float_of_string lxm)}
	| '\"' ([^'\"']* as lxm) '\"' { STRING_LITERAL(lxm) }
	| '\'' ([' '-'&' '('-'[' ']'-'~'] as lxm) '\'' { CHAR_LITERAL(lxm) }
    | ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
    | eof { EOF }

and comment = parse
    | "*/" { token lexbuf }
    | _    { token lexbuf }
