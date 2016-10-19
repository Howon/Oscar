{ open Parser }

rule token = parse
    | [' ' '\t' '\r' '\n']  { token lexbuf }
    | "/*"                  { comment lexbuf }

    (* braces/parens/brackets *)
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '{'                   { LBRACE }
    | '}'                   { RBRACE }
    | '['                   { LBRACKET }
    | ']'                   { RBRACKET }

    (* math *)
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIVIDE }
    | '='                   { ASIGN }
    | '%'                   { MOD }

    (* comma, dot, semi *)
    | '.'                   { DOT }
    | ','                   { COMMA }
    | ';'                   { SEMI }

    (* logic *)
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | '<'                   { LT }
    | "<="                  { LEQ }
    | '>'                   { GT }
    | ">="                  { GEQ }
    | "&&"                  { AND }
    | "||"                  { OR }
    | "true"                { TRUE }
    | "false"               { FALSE }

    (* bit math *)
    | '&'                   { BITWISE_AND }
    | '|'                   { BITWISE_OR }
    | '^'                   { BITWISE_XOR }
    | '~'                   { BITWISE_NOT }
    | ">>"                  { BITSHIFT_RIGHT }
    | "<<"                  { BITSHIFT_LEFT }

    (* functions *)
    | ':'                   { FUNCTION_TYPE }
    | "->"                  { ARROW }
    | "=>"                  { FUNCTION }
    | "return"              { RETURN }

    (* flow control *)
    | "if"                  { IF }
    | "else"                { ELSE }
    | "for"                 { FOR }
    | "to"                  { LOOP_TO }
    | "by"                  { LOOP_INC }

    (* actors *)
    | "sender"              { SENDER }
    | "die"                 { DIE }
    | "spawn"               { SPAWN }
    | "|>"                  { SEND }
    | "|>>"                 { BROADCAST }

    (* mutability for actors *)
    | "mut"                 { MUTABLE }

    (* primitive types *)
    | "int"                 { INT }
    | "double"              { DOUBLE }
    | "char"		    { CHAR }
    | "bool"                { BOOL }
    | "unit"                { UNIT }

    (* non-primitive types *)
    | "string"              { STR }
    | "MAYBE"               { OPTION_MAYBE }
    | "NONE"                { OPTION_NONE }
    | "SOME"                { OPTION_SOME }
    | "list"                { LIST }
    | "set"                 { SET }
    | "map"                 { MAP }
    | "tup"                 { TUPLE }
    | "message"             { MESSAGE }
    | "actor"               { ACTOR }
    | "pool"                { POOL }
    | "def"                 { DEF }

    (* literals *)
    | ['0'-'9']+ as lxm { LITERAL(int_of_string lit) }
    | ['0'-'9']+'.'['0'-'9']* as lxm { DOUBLE_LIT(float_of_string lxm)}
    | '\"' ([^'\"']* as lxm) '\"' { STRING_LIT(lxm) }
    | '\'' (('a'-'z'|'A'-'Z')|'\\'['\\' '*' 'n' 'r' 't' '"' '''] as lxm) '\'' { CHAR_LIT(lxm_ }

    (* identifiers *)
    | ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
    | eof { EOF }

and comment = parse
    | "*/" { token lexbuf }
    | _    { token lexbuf }
