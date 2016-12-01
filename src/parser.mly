%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token ARITH_PLUS ARITH_MINUS ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%token ASSIGN
%token PUNC_COMMA PUNC_SEMI
%token LOGIC_EQ LOGIC_NEQ LANGLE RANGLE LOGIC_LEQ LOGIC_GEQ LOGIC_NOT
%token LOGIC_AND LOGIC_OR LOGIC_TRUE LOGIC_FALSE
%token BITWISE_AND BITWISE_OR BITWISE_XOR  BITWISE_RIGHT BITWISE_LEFT
%token FUNC_ARG_TYPE ARROW FUNC_RET_TYPE RETURN
%token FLOW_IF FLOW_ELSE FLOW_WHILE FLOW_FOR LOOP_TO LOOP_BY LOOP_FROM
%token ACT_SENDER ACT_DIE ACT_SPAWN ACT_RECEIVE ACT_SEND ACT_BROADCAST
%token MUTABLE
%token TYPE_INT TYPE_DOUBLE TYPE_CHAR TYPE_BOOL TYPE_UNIT TYPE_STR
%token TYPE_LIST TYPE_SET TYPE_MAP TYPE_TUPLE
%token TYPE_MESSAGE TYPE_ACTOR TYPE_POOL TYPE_FUNC
%token <int> INT_LIT
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
%left LANGLE RANGLE LOGIC_LEQ LOGIC_GEQ
%left BITWISE_AND BITWISE_XOR BITWISE_OR
%right BITWISE_LEFT BITWISE_RIGHT
%left ARITH_PLUS ARITH_MINUS
%left ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%right LOGIC_NOT
%left ACT_SEND ACT_BROADCAST

%right NEG /* for negative numbers */

%start program
%type <Ast.program> program

%%

program:
  messages actors functions EOF { ($1, $2, $3) }

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
  TYPE_MESSAGE ID LPAREN formals_opt RPAREN   { { m_name = $2; m_formals = $4 } }

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
      actor_stmt_opt functions receive RBRACE
        { { a_name = $2; a_formals = $4; a_body = $7;
            a_functions = $8; a_receive = $9 } }

/**********
FUNCTIONS
***********/

functions:
  /* nothing */               { [] }
  | function_list             { List.rev $1 }

function_list:
    fdecl                     { [$1] }
  | function_list fdecl       { $2::$1 }

fdecl:
  | TYPE_FUNC ID LPAREN formals_opt RPAREN FUNC_RET_TYPE typ
    ASSIGN LBRACE stmts RBRACE
      { { f_name = $2; f_formals = $4;
      f_return_t = $7; f_body = $10 } }

formals_opt:
  /* nothing */   { [] }
  | formal_list   { List.rev $1 }

formal_list:
  ID FUNC_ARG_TYPE typ                            { [($1, $3)] }
  | formal_list PUNC_COMMA ID FUNC_ARG_TYPE typ   { ($3, $5) :: $1 }

typ_opt:
  /* nothing */     { [] }
  | typ_list        { List.rev $1 }

typ_list:
  | typ                       { [$1] }
  | typ_list PUNC_COMMA typ   { $3 :: $1 }

/* primative types */
typ:
  simple_typ    { $1 }
  | cont_typ   { $1 }

simple_typ:
    TYPE_INT       { Int_t }
  | TYPE_BOOL     { Bool_t }
  | TYPE_DOUBLE   { Double_t }
  | TYPE_CHAR     { Char_t }
  | TYPE_UNIT     { Unit_t }

cont_typ:
  TYPE_STR      { String_t }
  | TYPE_MAP LANGLE typ PUNC_COMMA typ RANGLE { Map_t($3, $5) }
  | TYPE_TUPLE LANGLE typ_opt RANGLE { Tuple_t($3) }
  | TYPE_SET LANGLE typ RANGLE { Set_t($3) }
  | TYPE_LIST LANGLE typ RANGLE { List_t($3) }

/* for pattern matching with receive */
receive:
  ACT_RECEIVE ASSIGN LBRACE pattern_opt RBRACE { $4 }

pattern_opt:
  /* nothing */   { [] }
  | pattern_list  { List.rev $1 }

pattern_list:
    pattern              { [$1] }
  | pattern_list pattern { $2::$1 }

pattern:
    BITWISE_OR ID LPAREN formals_opt RPAREN FUNC_RET_TYPE
      LBRACE stmts RBRACE
            { { p_message_id = $2; p_message_formals = $4; p_stmts = $8; } }

mut_vdecl:
/* nothing */ { Continue }
  | MUTABLE typ ID { Mutdecl({ mv_name = $3;
                               mv_type = $2;
                               mv_init = Noexpr}) }
  | MUTABLE typ ID ASSIGN expr { Mutdecl({ mv_name = $3;
                                           mv_type = $2;
                                           mv_init = $5}) }

actor_spawn:
  TYPE_ACTOR LANGLE ID RANGLE ID ASSIGN ACT_SPAWN TYPE_ACTOR LANGLE ID RANGLE
      LPAREN actuals_opt RPAREN       { Spawn_act($3, $5, $10, $13) }
  | TYPE_POOL LANGLE ID RANGLE ID ASSIGN ACT_SPAWN TYPE_POOL LANGLE ID RANGLE
      LPAREN actuals_opt RPAREN       { Spawn_pool($3, $5, $10, $13) }

actor_stmt:
  stmt                  { $1 }
  | mut_vdecl PUNC_SEMI { $1 }

actor_stmt_opt:
  /* nothing */     { [] }
  | actor_stmt_list { List.rev $1 }

actor_stmt_list:
  actor_stmt                    { [$1] }
  | actor_stmt_list actor_stmt  { $2 :: $1 }

stmts:
  /* nothing */       { [] }
  | stmt_list         { List.rev $1 }

stmt_list:
  stmt       { [$1] }
  | stmt_list stmt    { $2 :: $1 }

stmt:
  expr PUNC_SEMI                                      { Expr $1 }
  | typ ID ASSIGN expr PUNC_SEMI                      { Vdecl({ v_name = $2;
                                                                v_type = $1;
                                                                v_init = $4}) }
  | actor_spawn PUNC_SEMI                             { $1 }
  | RETURN PUNC_SEMI                                  { Return Noexpr }
  | RETURN expr PUNC_SEMI                             { Return $2 }
  | LBRACE stmts RBRACE                               { Block($2) }
  | stmt_cond                                         { $1 }
  | stmt_iter                                         { $1 }

stmt_iter:
  FLOW_FOR LPAREN TYPE_INT ID LOOP_FROM INT_LIT LOOP_TO INT_LIT LOOP_BY
      INT_LIT RPAREN LBRACE stmts RBRACE  { For($4, $6, $8, $10, $13) }
  | FLOW_WHILE LPAREN expr RPAREN
      LBRACE stmts RBRACE                 { While($3, $6) }

stmt_cond:
  FLOW_IF LPAREN expr RPAREN LBRACE stmts RBRACE %prec NOELSE
        { If($3, $6, []) }
  | FLOW_IF LPAREN expr RPAREN LBRACE stmts RBRACE
        FLOW_ELSE LBRACE stmts RBRACE { If($3, $6, $10) }

map_opt:
  /* nothing */   { [] }
  | map_list      { List.rev $1 }

map_list:
  expr ARROW expr                         { [($1, $3)] }
  | map_list PUNC_COMMA expr ARROW expr   { ($3, $5) :: $1 }

cont_lit:
  TYPE_LIST LANGLE typ RANGLE
        LBRACKET actuals_opt RBRACKET             { List_Lit($3, $6) }
  | TYPE_SET LANGLE typ RANGLE
        LBRACKET actuals_opt RBRACKET             { Set_Lit($3, $6) }
  | TYPE_MAP LANGLE typ PUNC_COMMA typ RANGLE
        LBRACKET map_opt RBRACKET                 { Map_Lit($3, $5, $8) }
  | TYPE_TUPLE LANGLE typ_opt RANGLE
        LPAREN actuals_opt RPAREN                 { Tuple_Lit($3, $6) }

expr:
  ID                                              { Id($1) }
  | INT_LIT                                       { Int_Lit($1) }
  | DOUBLE_LIT                                    { Double_Lit($1) }
  | CHAR_LIT                                      { Char_Lit($1) }
  | STRING_LIT                                    { String_Lit($1) }
  | BOOL_LIT                                      { Bool_Lit($1) }
  | TYPE_UNIT                                     { Unit_Lit() }
  | LOGIC_TRUE                                    { Bool_Lit(true) }
  | LOGIC_FALSE                                   { Bool_Lit(false) }
  | cont_lit                                      { $1 }
  | expr ARITH_PLUS     expr                      { Binop($1, Add, $3) }
  | expr ARITH_MINUS    expr                      { Binop($1, Sub, $3) }
  | expr ARITH_TIMES    expr                      { Binop($1, Mult, $3) }
  | expr ARITH_DIVIDE   expr                      { Binop($1, Div, $3) }
  | expr ARITH_MOD      expr                      { Binop($1, Mod, $3) }
  | expr LOGIC_EQ       expr                      { Binop($1, Equal, $3) }
  | expr LOGIC_NEQ      expr                      { Binop($1, Neq, $3) }
  | expr LANGLE         expr                      { Binop($1, Less, $3) }
  | expr LOGIC_LEQ      expr                      { Binop($1, Leq, $3) }
  | expr RANGLE         expr                      { Binop($1, Greater,$3) }
  | expr LOGIC_GEQ      expr                      { Binop($1, Geq, $3) }
  | expr LOGIC_AND      expr                      { Binop($1, And, $3) }
  | expr LOGIC_OR       expr                      { Binop($1, Or, $3) }
  | expr BITWISE_AND    expr                      { Binop($1, Bit_And, $3) }
  | expr BITWISE_OR     expr                      { Binop($1, Bit_Or, $3) }
  | expr BITWISE_XOR    expr                      { Binop($1, Bit_Xor, $3) }
  | expr BITWISE_RIGHT  expr                      { Binop($1, Bit_RShift, $3)}
  | expr BITWISE_LEFT   expr                      { Binop($1, Bit_LShift, $3)}
  | ARITH_MINUS expr %prec NEG                    { Unop(Neg, $2) }
  | LOGIC_NOT expr                                { Unop(Not, $2) }
  | ID LPAREN actuals_opt RPAREN                  { Call($1, $3) }
  | LPAREN expr RPAREN                            { $2 }
  /* TODO: These need real actions, and to be put in the AST! */
  | ID LPAREN actuals_opt RPAREN ACT_SEND ID      { Noexpr }
  | ID LPAREN actuals_opt RPAREN ACT_BROADCAST ID { Noexpr }
  | lambda                                        { $1 }
  | ID ASSIGN expr                                { Assign($1, $3) }
  | ID LBRACKET expr RBRACKET                     { Access($1, $3) }

lambda:
  LPAREN formals_opt RPAREN FUNC_RET_TYPE typ ASSIGN LBRACE stmts RBRACE
        { Lambda({ l_formals = $2; l_return_t = $5; l_body = $8; }) }

actuals_opt:
  /* nothing */   { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
  expr                            { [$1] }
  | actuals_list PUNC_COMMA expr  { $3 :: $1 }
