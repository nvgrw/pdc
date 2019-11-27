
/*
 * Significant inspiration taken from this tutorial:
 * https://medium.com/@aleksandrasays/tutorial-parsing-json-with-ocaml-579cc054924f
 */

%token EOF
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

%start <Common.AST.program> program
%type  <Common.AST.block> block
%type  <Common.AST.decl> decl
%type  <Common.AST.typ> typ
%type  <Common.AST.stmt> stmt
%type  <Common.AST.loc> loc
%type  <Common.AST.expr> bool join equality rel expr term unary factor

/* Address Dangling Else */
%nonassoc GROUP_CLSE
%nonassoc ELSE

%%

program: 
  | block EOF { $1 }
  ;

block:
  | SCOPE_OPEN d = decls s = stmts SCOPE_CLSE { Block (List.rev d, List.rev s) }
  ;

decls:
  | rest = decls cur = decl   { cur :: rest }
  | (* empty *)               { [] }
  ;

decl:
  | t = typ name = IDENT STAT_SEPA { Decl (t, name) }
  ;

typ:
  | t = typ DEREF_OPEN size = NUM DEREF_CLSE  { Array (t, size) }
  | INT                                       { Int }
  | FLOAT                                     { Float }
  | CHAR                                      { Char }
  | BOOL                                      { Bool }
  ;

stmts:
  | rest = stmts cur = stmt { cur :: rest }
  | (* empty *)             { [] }
  ;

stmt:
  | l = loc ASSIGN e = bool STAT_SEPA                             { Assign (l, e) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt                    { If (e, t, None) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt ELSE f = stmt      { If (e, t, Some f) }
  | WHILE GROUP_OPEN e = bool GROUP_CLSE body = stmt              { While (e, body) }
  | DO body = stmt WHILE GROUP_OPEN e = bool GROUP_CLSE STAT_SEPA { Do (e, body) }
  | BREAK STAT_SEPA                                               { Break }
  | block                                                         { BlockStmt $1 }
  ;

loc:
  | l = loc DEREF_OPEN e = bool DEREF_CLSE  { Deref (l, e) }
  | name = IDENT                            { Id name }
  ;

bool:
  | l = bool OR r = join  { BinOp (l, Or, r) }
  | join                  { $1 }
  ;

join:
  | l = join AND r = equality { BinOp (l, And, r) }
  | equality                  { $1 }
  ;

equality:
  | l = equality EQ r = rel   { BinOp (l, Eq, r) }
  | l = equality NEQ r = rel  { BinOp (l, Neq, r) }
  | rel                       { $1 }
  ;

rel:
  | l = expr LT r = expr  { BinOp (l, Lt, r) }
  | l = expr LEQ r = expr { BinOp (l, Leq, r) }
  | l = expr GEQ r = expr { BinOp (l, Geq, r) }
  | l = expr GT r = expr  { BinOp (l, Gt, r) }
  | expr                  { $1 }
  ;

expr:
  | l = expr PLUS r = term  { BinOp (l, Add, r) }
  | l = expr MINUS r = term { BinOp (l, Subtract, r) }
  | term                    { $1 }
  ;

term:
  | l = term MULTIPLY r = unary { BinOp (l, Multiply, r) }
  | l = term DIVIDE r = unary   { BinOp (l, Divide, r) }
  | unary                       { $1 }
  ;

unary:
  | NOT e = unary   { UnOp (Not, e) }
  | MINUS e = unary { UnOp (Negate, e) }
  | factor          { $1 }
  ;

factor:
  | GROUP_OPEN bool GROUP_CLSE  { $2 }
  | loc                         { Var $1 }
  | n = NUM                     { Const (Num n) }
  | r = REAL                    { Const (Real r) }
  | TRUE                        { Const (Bool true) }
  | FALSE                       { Const (Bool false) }
  ;