%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token ARITH_PLUS ARITH_MINUS ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%token ASSIGN
%token PUNC_DOT PUNC_COMMA PUNC_SEMI
%token LOGIC_EQ LOGIC_NEQ LOGIC_LT LOGIC_LEQ LOGIC_GT LOGIC_GEQ LOGIC_AND LOGIC_OR LOGIC_TRUE LOGIC_FALSE
%token BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_NOT BITWISE_RIGHT BITWISE_LEFT
%token FUNCTION_ARG_TYPE ARROW FUNC_RET_TYPE FUNC_RETURN
%token FLOW_IF FLOW_ELSE FOR LOOP_TO LOOP_INC
%token ACTOR_ACTOR_SENDER ACTOR_DIE ACTOR_SPAWN ACTOR_RECEIVE BITWISE_OR ACTOR_SEND ACTOR_BROADCAST
%token MUTABLE
%token TYPE_INT TYPE_DOUBLE TYPE_CHAR TYPE_BOOL TYPE_UNIT
%token TYPE_STR TYPE_OPTION_MAYBE TYPE_OPTION_SOME TYPE_LIST TYPE_SET TYPE_MAP TYPE_TUPLE TYPE_MESSAGE TYPE_ACTOR TYPE_POOL TYPE_DEF TYPE_LET
%token <int> LITERAL
%token <float> DOUBLE_LIT
%token <string> STRING_LIT
%token <string> ID
%token <char> CHAR_LIT
%token EOF

%nonassoc NOELSE
%nonassoc PATTERN
%right ASSIGN
%left FUNCTION_ARG_TYPE ARROW FUNCTION_RET_TYPE
%left LOGIC_AND LOGIC_OR
%left LOGIC_EQ LOGIC_NEQ
%left LOGIC_LT LOGIC_GT LOGIC_LEQ LOGIC_GEQ
%left BITWISE_AND BITWISE_XOR BITWISE_OR
%right BITWISE_LEFT BITWISE_RIGHT
%left ARITH_PLUS ARITH_MINUS
%left ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%right BITWISE_NOT
%left ACTOR_SEND ACTOR_BROADCAST
%left PUNC_DOT

%right NEG /* for negative numbers */

%start program
%type <Ast.program> program

%%

program:
	decls EOF { $1 }

/* TODO: this does not build up a list properly */
/* how do we do this with three lists? */
decls:
    /* nothing */ { [], [], [] }
  | decls message_decl 		{ $2, $1 }
  | decls actor_decl 		{ $2, $1 }
  | decls fdecl 			{ $2, $1 }


message_decl:
	TYPE_MESSAGE ID LPAREN formals_opt RPAREN { None }

actor_decl:
  	TYPE_ACTOR ID LPAREN formals_opt RPAREN LBRACE mut_vdecl_list fdecl_list receive RBRACE { $2 }

fdecl_list:
	/* nothing */ 		{ [] }
  | fdecl_list fdecl 	{ List.rev $1 }

/* Q: do functions need to FUNC_RETURN explictly? */
/* TODO: right now functions need to have variables declared before statements */
fdecl:
    TYPE_DEF ID LPAREN formals_opt RPAREN FUNC_RET_TYPE typ ASSIGN LBRACE stmt_list RBRACE
    	{ { func_name = $2; function_return_t = $7; function_formals = $4; function_body = $10 } }
  | TYPE_DEF ID LPAREN formals_opt RPAREN FUNCTION_ARG_TYPE typ FUNC_RET_TYPE expr
  		{ { func_name = $2; function_return_t = $7; function_formals = $4; function_body = $9 }  }


formals_opt:
    /* nothing */ 	{ [] }
  | formal_list 	{ List.rev $1 }

formal_list:
    ID FUNCTION_ARG_TYPE typ 								{ [($1, $3)] }
  | formal_list PUNC_COMMA ID FUNCTION_ARG_TYPE typ 		{ ($3, $5) :: $1 }



typ_list:
	 /* nothing */
  | typ 					{ None }
  | typ_list PUNC_COMMA typ { List.rev $1 }


/* primative types */
typ:
    TYPE_INT 		{ Int_t }
  | TYPE_BOOL 		{ Bool_t }
  | TYPE_DOUBLE  	{ Double_t }
  | TYPE_CHAR		{ Char_t }
  | TYPE_UNIT		{ Unit_t }



vdecl_list:
	/* nothing */		{ [] }
  | vdecl_list vdecl 	{ $2 :: $1 }

vdecl:
	TYPE_LET typ ID PUNC_SEMI			{ $2, $3 }
	/* Q: do pools count as variable declarations? */


/* allows for mutables or immutables */
mut_vdecl_list:
	/* nothing */				{ [] }
  | mut_vdecl_list mut_decl 	{ $2 :: $1 }
  | mut_vdecl_list vdecl 		{ $2 :: $1 }

mut_decl:
	MUTABLE typ ID PUNC_SEMI		{ $2, $3 }


/* for set and list */
/* NOTE: only for declarations, not declarations and assignments */
set_list_decl:
	TYPE_LIST LOGIC_LT typ LOGIC_GT ID ASSIGN TYPE_LIST LOGIC_LT typ LOGIC_GT LBRACKET RBRACKET PUNC_SEMI { None }
  | TYPE_SET LOGIC_LT typ LOGIC_GT ID ASSIGN TYPE_SET LOGIC_LT typ LOGIC_GT LBRACKET RBRACKET PUNC_SEMI { None }

/* NOTE: there isn't a type check for these types matching up, but i think that's ok */
map_decl:
	TYPE_MAP LOGIC_LT typ PUNC_COMMA typ LOGIC_GT ID ASSIGN TYPE_MAP LOGIC_LT typ LOGIC_GT LBRACKET RBRACKET PUNC_SEMI { None }

tuple_decl:
	TYPE_TUPLE LOGIC_LT typ_list LOGIC_GT ID ASSIGN TYPE_TUPLE LOGIC_LT typ_list LOGIC_GT LBRACKET RBRACKET PUNC_SEMI { None }


/* for pattern matching with receive */
receive:
	ACTOR_RECEIVE LBRACE pattern_opt RBRACE { None }

pattern_opt:
	/* nothing */ 	{ [] }
  | pattern_list 	{ List.rev $1 }

/* NOTE: patterns end with statements, which end with PUNC_SEMIcolons */
pattern_list: 
	BITWISE_OR ID LPAREN formals_opt RPAREN FUNC_RET_TYPE stmt_list %prec PATTERN { [($2, $4, $7)] }
   | pattern_list BITWISE_OR ID LPAREN formals_opt RPAREN FUNC_RET_TYPE stmt_list { [($3, $5, $8)] :: $1 }




stmt_list:
	/* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

/* TODO: because FUNC_RETURNs are allowed here, it means they are allowed within patterns
			which we don't want, see Q on line 67 ish */
/* NOTE: statements must end with a PUNC_SEMI */
/* NOTE: one shift reduce conflict?????? */
stmt:
	expr PUNC_SEMI 									{ Expr $1 }
  | FUNC_RETURN PUNC_SEMI 							{ FUNC_RETURN Noexpr }
  | FUNC_RETURN expr PUNC_SEMI 						{ FUNC_RETURN $2 }
  | LBRACE stmt_list RBRACE 						{ Block(List.rev $2) }
  | FLOW_IF LPAREN expr RPAREN stmt %prec NOELSE 	{ FLOW_IF($3, $5, Block([])) }
  | FLOW_IF LPAREN expr RPAREN stmt FLOW_ELSE stmt 	{ FLOW_IF($3, $5, $7) }
 /*  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt */
  /* TODO: define for */
  /* NOTE: FLOW_IF, FLOW_ELSE, for are not defined in our LRM */

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ID          	  		{ Id($1) } 
  | LITERAL           		{ Lit($1) }
  | LOGIC_TRUE			  	{ BoolLit(true) }
  | LOGIC_FALSE				{ BoolLit(false) }
  | expr ARITH_PLUS   expr 	{ bin_op($1, Add,    $3) }
  | expr ARITH_MINUS  expr 	{ bin_op($1, Sub,    $3) }
  | expr ARITH_TIMES  expr 	{ bin_op($1, Mult,   $3) }
  | expr ARITH_DIVIDE expr 	{ bin_op($1, Div,    $3) }
  | expr ARITH_MOD	expr 	{ bin_op($1, Mod,	$3) }
  | expr LOGIC_EQ     expr 	{ bin_op($1, Equal, 	$3) }
  | expr LOGIC_NEQ    expr 	{ bin_op($1, Neq,   	$3) }
  | expr LOGIC_LT     expr 	{ bin_op($1, Less,  	$3) }
  | expr LOGIC_LEQ    expr 	{ bin_op($1, Leq,   	$3) }
  | expr LOGIC_GT     expr 	{ bin_op($1, Greater,$3) }
  | expr LOGIC_GEQ    expr 	{ bin_op($1, Geq,   	$3) }
  | expr LOGIC_AND    expr 	{ bin_op($1, And,   	$3) }
  | expr LOGIC_OR     expr 	{ bin_op($1, Or,    	$3) }
  /* | MINUS expr %prec NEG 				{ Unop(Neg, $2) } */
  /* | NOT expr         				{ Unop(Not, $2) } */
  | ID ASSIGN expr   					{ Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN		{ Call($1, $3) }
  | LPAREN expr RPAREN 					{ $2 }
  | ID LPAREN actuals_opt RPAREN ACTOR_SEND ID 		{ None }
  | ID LPAREN actuals_opt RPAREN ACTOR_BROADCAST ID 	{ None }
  /* TODO: implement ACTOR_BROADCASTing lists of messages */
  /* NOTE: negation, eg !a, does not exist in our LRM */

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list PUNC_COMMA expr { $3 :: $1 }
