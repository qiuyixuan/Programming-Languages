%{
  open Ast
  open Printf
  open Lexing
%}

%token <string> STRING VAR
%token PLUS EQUALS DOT LAMBDA LET IN EOF QUOTE LPAREN RPAREN CALL THEN

%type <Ast.source_exp> s_exp
%type <Ast.target_exp> t_exp

%start s_exp
%start t_exp
 
%%

s_exp : LET VAR EQUALS s_exp IN s_exp   { SE_Let($2,$4,$6) }
    | s_lexp                            { $1 }

s_lexp : LAMBDA VAR DOT s_exp          { SE_Lam ($2,$4) }
    | s_cexp                            { $1 }

s_cexp : s_cexp PLUS s_bexp             { SE_Concat($1,$3) }
     | s_bexp                           { $1 }

s_bexp : CALL STRING LPAREN s_exp RPAREN {SE_Call ($2, $4) }
    | s_appexp                          { $1 }

s_appexp : s_appexp s_aexp              { SE_App($1,$2) }
    | s_aexp                            { $1 }

s_aexp: VAR                             { SE_Var $1 }
    | STRING                            { SE_Str $1 }
    | LPAREN s_exp RPAREN               { $2 }


t_exp : LET VAR EQUALS t_exp IN t_exp   { TE_Let($2,$4,$6) }
    | t_lexp                            { $1 }

t_lexp : LAMBDA VAR DOT t_exp          { TE_Lam ($2,$4) }
    | CALL STRING LPAREN t_exp RPAREN THEN t_exp
                                        { TE_Call ($2, $4, $7) }
    | t_cexp                            { $1 }

t_cexp : t_cexp PLUS t_appexp           { TE_Concat($1,$3) }
     | t_appexp                         { $1 }

t_appexp : t_appexp t_aexp              { TE_App($1,$2) }
    | t_aexp                            { $1 }

t_aexp: VAR                             { TE_Var $1 }
    | STRING                            { TE_Str $1 }
    | LPAREN t_exp RPAREN               { $2 }
