%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token COMMA DOT SEMI
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token BITWISE_AND BITWISE_OR BITWISE_NOT BITSHIFT_RIGHT BITSHIFT_LEFT
%token IF ELSE FOR LOOP_TO LOOP_INC
%token ARROW FUNCTION_TYPE FUNCTION RETURN
%token SEND BROADCAST SENDER DIE SPAWN
%token OPTION_MAYBE OPTION_NONE OPTION_SOME
%token INT DOUBLE BOOL CHAR UNIT
%token STR LIST SET MAP TUPLE ACTOR MESSAGE POOL DEF

%token <int> LITERAL
%token <string> ID
%token <string> STRING_LIT
%token <string> CHAR_LIT
%token <float> DOUBLE_LIT
%token EOF

%right ASSIGN
%left FUNCTION_TYPE ARROW FUNCTION
%left AND OR
%right BITWISE_AND BITWISE_OR
%nonassoc EQ NEQ LT LEQ GT GEQ
%right BITSHIFT_RIGHT BITSHIFT_LEFT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right BITWISE_NOT
%left SEND BROADCAST
%left DOT

(* DONE THROUGH HERE *)

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
