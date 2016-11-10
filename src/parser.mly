%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token ARITH_PLUS ARITH_MINUS ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%token ASSIGN
%token PUNC_DOT PUNC_COMMA PUNC_SEMI
%token LOGIC_EQ LOGIC_NEQ LANGLE_BRACKET LOGIC_LEQ RANGLE_BRACKET LOGIC_GEQ
%token LOGIC_AND LOGIC_OR LOGIC_TRUE LOGIC_FALSE
%token BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_NOT BITWISE_RIGHT BITWISE_LEFT
%token FUNC_ARG_TYPE ARROW FUNC_RET_TYPE RETURN
%token FLOW_IF FLOW_ELSE FLOW_WHILE FLOW_FOR LOOP_TO LOOP_BY LOOP_FROM
%token ACT_SENDER ACT_DIE ACT_SPAWN ACT_RECEIVE ACT_SEND ACT_BROADCAST
%token MUTABLE
%token TYPE_INT TYPE_DOUBLE TYPE_CHAR TYPE_BOOL TYPE_UNIT
%token TYPE_STR TYPE_MAYBE TYPE_SOME TYPE_NONE TYPE_LIST TYPE_SET TYPE_MAP
%token TYPE_TUPLE TYPE_MESSAGE TYPE_ACTOR TYPE_POOL TYPE_DEF
%token <int> LITERAL
%token <float> DOUBLE_LIT
%token <string> STRING_LIT
%token <char> CHAR_LIT
%token <bool> BOOL_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%right ASSIGN
%left FUNC_ARG_TYPE ARROW FUNC_RET_TYPE
%left LOGIC_AND LOGIC_OR
%left LOGIC_EQ LOGIC_NEQ
%left LANGLE_BRACKET RANGLE_BRACKET LOGIC_LEQ LOGIC_GEQ
%left BITWISE_AND BITWISE_XOR BITWISE_OR
%right BITWISE_LEFT BITWISE_RIGHT
%left ARITH_PLUS ARITH_MINUS
%left ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%right BITWISE_NOT
%left ACT_SEND ACT_BROADCAST
%left PUNC_DOT

%right NEG /* for negative numbers */

%start program
%type <Ast.program> program

%%

program:
  messages actors functions EOF { Program($1, $2, $3) }


/**********
MESSAGES
***********/

messages:
  /* nothing */               { [] }
  | message_list              { List.rev $1 }

message_list:
    message_decl              { [$1] }
  | message_list message_decl { $2::$1 }

message_decl:
  TYPE_MESSAGE ID LPAREN formals_opt RPAREN   { { name = $2; formals = $4 } }


/**********
ACTORS
***********/

actors:
  /* nothing */               { [] }
  | actor_list                { List.rev $1 }

actor_list:
    actor_decl                { [$1] }
  | actor_list actor_decl     { $2::$1 }

actor_decl:
  TYPE_ACTOR ID LPAREN formals_opt RPAREN LBRACE
      mut_vdecl_list functions receive RBRACE
        { { name = $2; formals = $4; body = $7;
            functions = $8; receive = $9 } }


/**********
FUNCTIONS
***********/

functions:
  /* nothing */               { [] }
  | function_list             { List.rev $1 }

function_list:
    fdecl                     { [$1] }
  | function_list fdecl       { $2::$1 }


/* TODO: implement lambda functs */
fdecl:
  | TYPE_DEF ID LPAREN formals_opt RPAREN FUNC_RET_TYPE typ
    ASSIGN LBRACE stmt_list RBRACE
      { { name = $2; formals = $4;
      return_t = $7; function_body = $10 } }

formals_opt:
  /* nothing */   { [] }
  | formal_list   { List.rev $1 }

formal_list:
  ID FUNC_ARG_TYPE typ                            { [($1, $3)] }
  | formal_list PUNC_COMMA ID FUNC_ARG_TYPE typ   { ($3, $5) :: $1 }

typ_list:
   /* nothing */
  | typ                       { None }
  | typ_list PUNC_COMMA typ   { List.rev $1 }


/* primative types */
/* TODO: handle MAYBE/SOME/NONE */
typ:
  simple_typ    { $1 }
  | fancy_typ   { $1 }

simple_typ:
  TYPE_INT        { Int_t }
  | TYPE_BOOL     { Bool_t }
  | TYPE_DOUBLE   { Double_t }
  | TYPE_CHAR     { Char_t }
  | TYPE_UNIT     { Unit_t }
  | TYPE_STR      { String_t }

fancy_typ:
  | TYPE_MAP LANGLE_BRACKET typ PUNC_COMMA typ RANGLE_BRACKET { map($3, $5) }
  | TYPE_TUPLE LANGLE_BRACKET typ_list RANGLE_BRACKET { tuple($3) }
  | TYPE_SET LANGLE_BRACKET typ RANGLE_BRACKET { set($3) }
  | TYPE_LIST LANGLE_BRACKET typ RANGLE_BRACKET { lst($3) }

vdecl_list:
  /* nothing */       { [] }
  | vdecl_list vdecl  { $2 :: $1 }

vdecl:
  simple_typ ID ASSIGN expr PUNC_SEMI   { $1, $2, $4 }
  | set_list_decl                       { $1 }
  | map_decl                            { $1 }
  | tuple_decl                          { $1 }
  /* Q: do pools count as variable declarations? */

vassign:
  typ ID ASSIGN expr  { $1, $2, $4 }
  | ID ASSIGN expr {$1, $3}

/* allows for mutables or immutables */
/* TODO: WE GOTTA MAKE MUTABLE MORE GENERALIZED */
/*       AKA mut map<str,int> NEEDS TO BE ACCEPTED */
mut_vdecl_list:
  /* nothing */               { [] }
  | mut_vdecl_list mut_decl   { $2 :: $1 }
  | mut_vdecl_list vdecl      { $2 :: $1 }

mut_decl:
  MUTABLE vdecl { $2 }

/* TODO: make assignments more generalized, or at least make for all types */
/* TODO: try to reduce the number of patterns for these different types */
set_list_decl:
  typ LANGLE_BRACKET typ RANGLE_BRACKET ID ASSIGN LBRACKET RBRACKET PUNC_SEMI                            { None }
  | TYPE_SET LANGLE_BRACKET   typ RANGLE_BRACKET   ID ASSIGN TYPE_SET LANGLE_BRACKET   typ
      RANGLE_BRACKET   LBRACKET RBRACKET PUNC_SEMI                            { None }

/* NOTE: there isn't a type check for these types matching up, but i think that's ok */
map_decl:
  TYPE_MAP LANGLE_BRACKET   typ PUNC_COMMA typ RANGLE_BRACKET   ID ASSIGN TYPE_MAP
      LANGLE_BRACKET   typ RANGLE_BRACKET   LBRACKET RBRACKET PUNC_SEMI               { None }

tuple_decl:
  TYPE_TUPLE LANGLE_BRACKET typ_list RANGLE_BRACKET ID ASSIGN TYPE_TUPLE LANGLE_BRACKET
      typ_list RANGLE_BRACKET   LBRACKET RBRACKET PUNC_SEMI                   { None }



/* for pattern matching with receive */
receive:
  ACT_RECEIVE ASSIGN LBRACE pattern_opt RBRACE { None }

pattern_opt:
  /* nothing */   { [] }
  | pattern_list  { List.rev $1 }

pattern_list:
    pattern              { [$1] }
  | pattern_list pattern { $2::$1 }

pattern:
    BITWISE_OR ID LPAREN formals_opt RPAREN FUNC_RET_TYPE 
      LBRACE stmt_list RBRACE
            { { message_id = $2; message_formals = $4; stmts = $8; } }


stmt_list:
  /* nothing */       { [] }
  | stmt_list stmt    { $2 :: $1 }

stmt:
  expr PUNC_SEMI                                      { Expr $1 }
  | RETURN PUNC_SEMI                                  { RETURN Noexpr }
  | RETURN expr PUNC_SEMI                             { RETURN $2 }
  | LBRACE stmt_list RBRACE                           { Block(List.rev $2) }
  | stmt_cond                                         { $1 }
  | stmt_iter                                         { $1 }

stmt_iter:
  FLOW_FOR LPAREN vdecl LOOP_FROM LITERAL LOOP_TO
      LITERAL LOOP_BY LITERAL RPAREN stmt   { For($3, $5, $7, $9, $11) }
  | FLOW_WHILE LPAREN expr RPAREN stmt      { While($3, $5) }

/* TODO: this causes a shift/reduce conflict */
stmt_cond:
  FLOW_IF LPAREN expr RPAREN stmt %prec NOELSE
        { FLOW_IF($3, $5, Block([])) }
  | FLOW_IF LPAREN expr RPAREN stmt FLOW_ELSE stmt { FLOW_IF($3, $5, $7) }

expr_opt:
  /* nothing */   { Noexpr }
  | expr          { $1 }

expr:
  ID                                                { Id($1) }
  | LITERAL                                         { Lit($1) }
  | DOUBLE_LIT                                      { Double_Lit($1) }
  | CHAR_LIT                                        { Char_Lit($1) }
  | STRING_LIT                                      { String_Lit($1) }
  | BOOL_LIT                                        { Bool_Lit($1) }
  | LOGIC_TRUE                                      { BoolLit(true) }
  | LOGIC_FALSE                                     { BoolLit(false) }
  | expr ARITH_PLUS   expr                          { bin_op($1, Add, $3) }
  | expr ARITH_MINUS  expr                          { bin_op($1, Sub, $3) }
  | expr ARITH_TIMES  expr                          { bin_op($1, Mult, $3) }
  | expr ARITH_DIVIDE expr                          { bin_op($1, Div, $3) }
  | expr ARITH_MOD    expr                          { bin_op($1, Mod, $3) }
  | expr LOGIC_EQ     expr                          { bin_op($1, Equal, $3) }
  | expr LOGIC_NEQ    expr                          { bin_op($1, Neq, $3) }
  | expr LANGLE_BRACKET       expr                  { bin_op($1, Less, $3) }
  | expr LOGIC_LEQ    expr                          { bin_op($1, Leq, $3) }
  | expr RANGLE_BRACKET       expr                  { bin_op($1, Greater,$3) }
  | expr LOGIC_GEQ    expr                          { bin_op($1, Geq, $3) }
  | expr LOGIC_AND    expr                          { bin_op($1, And, $3) }
  | expr LOGIC_OR     expr                          { bin_op($1, Or, $3) }
  /* | MINUS expr %prec NEG                         { Unop(Neg, $2) } */
  /* | NOT expr                                     { Unop(Not, $2) } */
  | ID LPAREN actuals_opt RPAREN                    { Call($1, $3) }
  | LPAREN expr RPAREN                              { $2 }
  | ID LPAREN actuals_opt RPAREN ACT_SEND ID        { None }
  | ID LPAREN actuals_opt RPAREN ACT_BROADCAST ID   { None }
  | lambda                                          { $1 }
  /* NOTE: negation, eg !a, does not exist in our LRM */

lambda:
  LPAREN LPAREN formals_opt RPAREN FUNC_RET_TYPE typ ASSIGN expr RPAREN
        { { formals = $3; return_t = $6;, return_expr = $8; } }
  | LPAREN LPAREN formals_opt RPAREN FUNC_RET_TYPE typ ASSIGN RETURN expr RPAREN
        { { formals = $3; return_t = $6;, return_expr = $9; } }

actuals_opt:
  /* nothing */   { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
  expr                            { [$1] }
  | actuals_list PUNC_COMMA expr  { $3 :: $1 }
