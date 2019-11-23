
%token SCOPE_OPEN
%token SCOPE_CLSE
%token DEREF_OPEN
%token DEREF_CLSE
%token GROUP_OPEN
%token GROUP_CLSE
%token STAT_SEPA
(* Misc *)
%token ASSIGN
%token <string> IDENT
(* Control Flow *)
%token IF
%token ELSE
%token WHILE
%token DO
%token BREAK
(* Relational *)
%token OR 
%token AND
%token EQ 
%token NEQ
%token LT 
%token LEQ 
%token GEQ 
%token GT
(* Arithmetic *)
%token PLUS 
%token MINUS 
%token MULTIPLY 
%token DIVIDE
(* Unary *)
%token NOT (* | NEGATE -- negate is ambiguous, will handle this in parsing *)
%token <int> NUM
%token <float> REAL
%token TRUE
%token FALSE
(* Types *)
%token INT 
%token FLOAT 
%token CHAR 
%token BOOL

%start <Program.program> program

%%

program: 
  | block { 0 }
  ;

block:
  | SCOPE_OPEN decls stmts SCOPE_CLSE { 0 }
  ;

decls:
  | decls decl { 0 }
  | (* empty *) { 0 }
  ;

decl:
  | typ IDENT { 0 }
  ;

typ:
  | typ DEREF_OPEN NUM DEREF_CLSE { 0 }
  | INT  { 0 }
  | FLOAT { 0 }
  | CHAR  { 0 }
  | BOOL { 0 }
  ;

stmts:
  | stmts stmt { 0 }
  | (* empty *) { 0 }
  ;

stmt:
  | loc ASSIGN bool STAT_SEPA { 0 }
  | IF GROUP_OPEN bool GROUP_CLSE { 0 }
  | IF GROUP_OPEN bool GROUP_CLSE ELSE stmt { 0 }
  | WHILE GROUP_OPEN bool GROUP_CLSE stmt { 0 }
  | DO stmt WHILE GROUP_OPEN bool GROUP_CLSE STAT_SEPA { 0 }
  | BREAK STAT_SEPA { 0 }
  | block { 0 }
  ;

loc:
  | loc DEREF_OPEN bool DEREF_CLSE { 0 }
  | IDENT { 0 }
  ;

bool:
  | bool OR join { 0 }
  | join { 0 }
  ;

join:
  | join AND equality { 0 }
  | equality { 0 }
  ;

equality:
  | equality EQ rel { 0 }
  | equality NEQ rel { 0 }
  | rel { 0 }
  ;

rel:
  | expr LT expr { 0 }
  | expr LEQ expr { 0 }
  | expr GEQ expr { 0 }
  | expr GT expr { 0 }
  | expr { 0 }
  ;

expr:
  | expr PLUS term { 0 }
  | expr MINUS term { 0 }
  | term { 0 }
  ;

term:
  | term MULTIPLY unary { 0 }
  | term DIVIDE unary { 0 }
  | unary { 0 }
  ;

unary:
  | NOT unary { 0 }
  | MINUS unary { 0 }
  | factor { 0 }
  ;

factor:
  | GROUP_OPEN bool GROUP_CLSE { 0 }
  | loc { 0 }
  | NUM { 0 }
  | REAL { 0 }
  | TRUE { 0 }
  | FALSE { 0 }
  ;