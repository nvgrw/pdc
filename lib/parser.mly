
%token EOF
%token SCOPE_OPEN
%token SCOPE_CLSE
%token DEREF_OPEN
%token DEREF_CLSE
%token GROUP_OPEN
%token GROUP_CLSE
%token STAT_SEPA
%token EXPR_SEPA
%token SELECT
(* Misc *)
%token ASSIGN
%token PROB_ASSIGN
%token PRINT
%token <string> IDENT
(* Control Flow *)
%token IF
%token ELSE
%token WHILE
%token DO
%token BREAK
%token CHOOSE
(* Relational *)
%token BAND
%token BXOR
%token BOR
%token AND
%token OR
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
%token REMAINDER
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
%type  <Common.Meta.meta Common.AST.expr> bool join bwor bwxor bwand equality rel expr term unary factor

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
  | INT GROUP_OPEN prec = NUM GROUP_CLSE      { Int (prec, Position $loc) }
  | INT                                       { Int (64, Position $loc) }
  | FLOAT                                     { Float (Position $loc) }
  | CHAR                                      { Char (Position $loc) }
  | BOOL                                      { Common.AST.Bool (Common.Meta.Position $loc) }
  ;

stmts:
  | rest = stmts cur = stmt { cur :: rest }
  | (* empty *)             { [] }
  ;

stmt:
  | l = loc ASSIGN e = bool STAT_SEPA                                 { Assign (l, e, Position $loc) }
  | l = loc PROB_ASSIGN SCOPE_OPEN e = expr_list SCOPE_CLSE STAT_SEPA { ProbAssign (l, List.rev e, Position $loc) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt                        { If (e, t, None, Position $loc) }
  | IF GROUP_OPEN e = bool GROUP_CLSE t = stmt ELSE f = stmt          { If (e, t, Some f, Position $loc) }
  | WHILE GROUP_OPEN e = bool GROUP_CLSE body = stmt                  { While (e, body, Position $loc) }
  | DO body = stmt WHILE GROUP_OPEN e = bool GROUP_CLSE STAT_SEPA     { Do (e, body, Position $loc) }
  | BREAK STAT_SEPA                                                   { Break (Position $loc) }
  | PRINT e = bool STAT_SEPA                                          { Print (e, Position $loc) }
  | CHOOSE c = choose STAT_SEPA                                       {
      let (stmts, probs) = c |> List.rev |> List.split in
      Choose (stmts, probs, Position $loc)
    }
  | block                                                             { BlockStmt ($1, Position $loc) }
  ;

choose:
  | rest = choose p = NUM SELECT s = stmt { (s, p) :: rest }
  | p = NUM SELECT s = stmt               { [(s, p)] }

expr_list:
  | rest = expr_list EXPR_SEPA e = expr { e :: rest }
  | e = expr                            { [e] }

loc:
  | l = loc DEREF_OPEN e = bool DEREF_CLSE  { Deref (l, e, Position $loc) }
  | name = IDENT                            { Id (name, Position $loc) }
  ;

bool:
  | l = bool OR r = join  { BinOp (l, Or (Position $loc($2)), r, Position $loc) }
  | join                  { $1 }
  ;

join:
  | l = join AND r = bwor { BinOp (l, And (Position $loc($2)), r, Position $loc) }
  | bwor                  { $1 }
  ;

/* Bitwise Operators
   These bind tighter than AND and OR. x & y && z = (x & y) && z */
bwor:
  | l = bwor BOR r = bwxor { BinOp (l, BOr (Position $loc($2)), r, Position $loc) }
  | bwxor                  { $1 }
  ;

bwxor:
  | l = bwxor BXOR r = bwand { BinOp (l, BXor (Position $loc($2)), r, Position $loc) }
  | bwand                    { $1 }
  ;

bwand:
  | l = bwand BAND r = equality { BinOp (l, BAnd (Position $loc($2)), r, Position $loc) }
  | equality                    { $1 }
  ;

/* ----------------- */

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
  | l = term MULTIPLY r = unary  { BinOp (l, Multiply (Position $loc($2)), r, Position $loc) }
  | l = term DIVIDE r = unary    { BinOp (l, Divide (Position $loc($2)), r, Position $loc) }
  | l = term REMAINDER r = unary { BinOp (l, Remainder (Position $loc($2)), r, Position $loc) }
  | unary                        { $1 }
  ;

unary:
  | NOT e = unary   { UnOp (Not (Position $loc($1)), e, Position $loc) }
  | MINUS e = unary { UnOp (Negate (Position $loc($1)), e, Position $loc) }
  | factor          { $1 }
  ;

factor:
  | GROUP_OPEN bool GROUP_CLSE  { $2 }
  | loc                         { Var ($1, Position $loc) }
  | n = NUM                     { Const (Num (n, -1, Position $loc), Position $loc) }
  | r = REAL                    { Const (Real (r, Position $loc), Position $loc) }
  | TRUE                        { Const (Bool (true, Position $loc), Position $loc) }
  | FALSE                       { Const (Bool (false, Position $loc), Position $loc) }
  ;