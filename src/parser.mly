%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Syntax 

let binop op arg1 arg2 =
  App(App(Var(op), arg1), arg2)

%}

%token <int> INT
%token TRUE FALSE
%token <string> IDENT
%token FUN ARROW
%token IF THEN ELSE
%token LET REC EQ IN
%token EOF
%token PLUS MINUS 
%token MUL 
%token LT LE GT GE EE NE
%token AND OR
%token LPAREN RPAREN
%token LBRACKET COMMA RBRACKET
%token CONS

%nonassoc EQ LT LE GT GE NE EE CONS COMMA OR AND 
%left PLUS MINUS
%left MUL

%start expr 
%type <Syntax.expr> expr

%%    

expr: LET IDENT EQ expr IN expr      { Let($2,$4,$6) }
    | LET REC IDENT EQ expr IN expr  { LetRec($3,$5,$7) }
    | FUN IDENT ARROW expr           { Fun($2,$4) }
    | IF expr THEN expr ELSE expr    { If($2,$4,$6) }
    | bin_expr                       { $1 }

bin_expr: bin_expr EQ bin_expr          { binop "=" $1 $3 }
    | bin_expr LT bin_expr              { binop "<" $1 $3 }
    | bin_expr LE bin_expr              { binop "<="$1 $3 }
    | bin_expr GT bin_expr              { binop ">" $1 $3 }
    | bin_expr GE bin_expr              { binop ">=" $1 $3 }
    | bin_expr NE bin_expr              { binop "!=" $1 $3 }
    | bin_expr EE bin_expr              { binop "==" $1 $3 }
    | bin_expr AND bin_expr             { binop "and" $1 $3 }
    | bin_expr OR bin_expr              { binop "or" $1 $3 }
    | bin_expr PLUS bin_expr            { binop "+" $1 $3 }
    | bin_expr MINUS bin_expr           { binop "-" $1 $3 }
    | bin_expr MUL bin_expr             { binop "*" $1 $3 }
    | array_expr                        { $1 }

array_expr: array_expr CONS array_expr     { binop "::" $1 $3 }
    | LBRACKET array_expr COMMA array_expr { binop "::" $2 $4 }
    | array_expr COMMA array_expr          { binop "::" $1 $3 }
    | array_expr RBRACKET                  { binop "::" $1 EmptyExpr }
    | LBRACKET RBRACKET                    { EmptyExpr }
    | app_expr                             { $1 }

app_expr: app_expr prim_expr { App($1,$2) }
    | prim_expr              { $1 }

prim_expr: LPAREN expr RPAREN  { $2 }   
    | INT                      { NumLit $1 }
    | TRUE                     { BoolLit true }
    | FALSE                    { BoolLit false }
    | IDENT                    { Var($1) }
