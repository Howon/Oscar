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
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | '<'                   { LT }
    | "<="                  { LEQ }
    | '>'                   { GT }
    | ">="                  { GEQ }
    | "&&"                  { AND }
    | "||"                  { OR }
    | '!'                   { NOT }
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
    | "list"                { LIST }
    | "MAYBE"               { OPTION_MAYBE }
    | "NONE"                { OPTION_NONE }
    | "SOME"                { OPTION_SOME }
    | ['0'-'9']+ as lxm { LITERAL(int_of_string lit) }
    | ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
    | eof { EOF }

and comment = parse
    | "*/" { token lexbuf }
    | _    { token lexbuf }
