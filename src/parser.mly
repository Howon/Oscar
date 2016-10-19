%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token IF ELSE FOR LOOP_TO LOOP_INC
%token DEF ARROW FUNCTION RETURN
%token SEND MESSAGE BROADCAST ACTOR SENDER POOL DIE SPAWN
%token OPTION_MAYBE OPTION_NONE OPTION_SOME
%token INT DOUBLE BOOL STR LIST SET MAP TUP

%token <int> LITERAL
%token <string> ID
%token <string> STRING_LIT
%token <string> CHAR_LIT
%token EOF

%left COMMA
%right ASN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
    | expr PLUS expr    { Binop($1, Add, $3) }
    | expr MINUS expr   { Binop($1, Sub, $3) }
    | expr TIMES expr   { Binop($1, Mul, $3) }
    | expr DIVIDE expr  { Binop($1, Div, $3) }
    | expr COMMA expr   { Seq($1, $3) }
    | VARIABLE ASN expr { Asn($1, $3) }
    | VARIABLE          { Var($1) } 
    | LITERAL           { Lit($1) }
