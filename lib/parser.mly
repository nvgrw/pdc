
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

%start <Common.Meta.meta Common.AST.program> program
%type  <Common.Meta.meta Common.AST.block> block
%type  <Common.Meta.meta Common.AST.decl> decl
%type  <Common.Meta.meta Common.AST.typ> typ
%type  <Common.Meta.meta Common.AST.stmt> stmt
%type  <Common.Meta.meta Common.AST.loc> loc
%type  <Common.Meta.meta Common.AST.expr> bool join equality rel expr term unary factor

/* Address Dangling Else */
%nonassoc GROUP_CLSE
%nonassoc ELSE

%%

program: 
  | block EOF { $1 }
  ;

block:
  | SCOPE_OPEN d = decls s = stmts SCOPE_CLSE { Block (Common.Data.StringMap.empty, List.rev d, List.rev s, Position $loc) }
  ;

decls:
  | rest = decls cur = decl   { cur :: rest }
  | (* empty *)               { [] }
  ;

decl:
  | t = typ name = IDENT STAT_SEPA { Decl (t, name, Position $loc) }
  ;

typ:
  | t = typ DEREF_OPEN size = NUM DEREF_CLSE  { Array (t, size, Position $loc) }
  | INT                                       { Int (Position $loc) }
  | FLOAT                                     { Float (Position $loc) }
  | CHAR                                      { Char (Position $loc) }
  | BOOL                                      { Bool (Position $loc) }
  ;

stmts:
  | rest = stmts cur = stmt { cur :: rest }
  | (* empty *)             { [] }
  ;

stmt:
  | l = loc ASSIGN e = bool STAT_SEPA                             { Assign (l, e, Position $loc) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt                    { If (e, t, None, Position $loc) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt ELSE f = stmt      { If (e, t, Some f, Position $loc) }
  | WHILE GROUP_OPEN e = bool GROUP_CLSE body = stmt              { While (e, body, Position $loc) }
  | DO body = stmt WHILE GROUP_OPEN e = bool GROUP_CLSE STAT_SEPA { Do (e, body, Position $loc) }
  | BREAK STAT_SEPA                                               { Break (Position $loc) }
  | block                                                         { BlockStmt ($1, Position $loc) }
  ;

loc:
  | l = loc DEREF_OPEN e = bool DEREF_CLSE  { Deref (l, e, Position $loc) }
  | name = IDENT                            { Id (name, Position $loc) }
  ;

bool:
  | l = bool OR r = join  { BinOp (l, Or (Position $loc($2)), r, Position $loc) }
  | join                  { $1 }
  ;

join:
  | l = join AND r = equality { BinOp (l, And (Position $loc($2)), r, Position $loc) }
  | equality                  { $1 }
  ;

equality:
  | l = equality EQ r = rel   { BinOp (l, Eq (Position $loc($2)), r, Position $loc) }
  | l = equality NEQ r = rel  { BinOp (l, Neq (Position $loc($2)), r, Position $loc) }
  | rel                       { $1 }
  ;

rel:
  | l = expr LT r = expr  { BinOp (l, Lt (Position $loc($2)), r, Position $loc) }
  | l = expr LEQ r = expr { BinOp (l, Leq (Position $loc($2)), r, Position $loc) }
  | l = expr GEQ r = expr { BinOp (l, Geq (Position $loc($2)), r, Position $loc) }
  | l = expr GT r = expr  { BinOp (l, Gt (Position $loc($2)), r, Position $loc) }
  | expr                  { $1 }
  ;

expr:
  | l = expr PLUS r = term  { BinOp (l, Add (Position $loc($2)), r, Position $loc) }
  | l = expr MINUS r = term { BinOp (l, Subtract (Position $loc($2)), r, Position $loc) }
  | term                    { $1 }
  ;

term:
  | l = term MULTIPLY r = unary { BinOp (l, Multiply (Position $loc($2)), r, Position $loc) }
  | l = term DIVIDE r = unary   { BinOp (l, Divide (Position $loc($2)), r, Position $loc) }
  | unary                       { $1 }
  ;

unary:
  | NOT e = unary   { UnOp (Not (Position $loc($1)), e, Position $loc) }
  | MINUS e = unary { UnOp (Negate (Position $loc($1)), e, Position $loc) }
  | factor          { $1 }
  ;

factor:
  | GROUP_OPEN bool GROUP_CLSE  { $2 }
  | loc                         { Var ($1, Position $loc) }
  | n = NUM                     { Const (Num (n, Position $loc), Position $loc) }
  | r = REAL                    { Const (Real (r, Position $loc), Position $loc) }
  | TRUE                        { Const (Bool (true, Position $loc), Position $loc) }
  | FALSE                       { Const (Bool (false, Position $loc), Position $loc) }
  ;