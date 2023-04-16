type token =
  | STRING of (string)
  | VAR of (string)
  | PLUS
  | EQUALS
  | DOT
  | LAMBDA
  | LET
  | IN
  | EOF
  | QUOTE
  | LPAREN
  | RPAREN
  | CALL
  | THEN

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Ast
  open Printf
  open Lexing
# 24 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* EQUALS *);
  261 (* DOT *);
  262 (* LAMBDA *);
  263 (* LET *);
  264 (* IN *);
    0 (* EOF *);
  265 (* QUOTE *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* CALL *);
  269 (* THEN *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\004\000\005\000\005\000\
\006\000\006\000\007\000\007\000\007\000\002\000\002\000\008\000\
\008\000\008\000\009\000\009\000\010\000\010\000\011\000\011\000\
\011\000\000\000\000\000"

let yylen = "\002\000\
\006\000\001\000\004\000\001\000\003\000\001\000\005\000\001\000\
\002\000\001\000\001\000\001\000\003\000\006\000\001\000\004\000\
\007\000\001\000\003\000\001\000\002\000\001\000\001\000\001\000\
\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\011\000\000\000\000\000\000\000\
\000\000\026\000\002\000\000\000\006\000\000\000\010\000\024\000\
\023\000\000\000\000\000\000\000\000\000\027\000\015\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
\013\000\000\000\005\000\000\000\000\000\025\000\000\000\000\000\
\003\000\000\000\000\000\016\000\000\000\000\000\000\000\007\000\
\000\000\000\000\001\000\014\000\000\000\017\000"

let yydgoto = "\003\000\
\010\000\022\000\011\000\012\000\013\000\014\000\015\000\023\000\
\024\000\025\000\026\000"

let yysindex = "\004\000\
\008\255\039\255\000\000\000\000\000\000\017\255\020\255\008\255\
\023\255\000\000\000\000\030\255\000\000\015\255\000\000\000\000\
\000\000\024\255\028\255\039\255\038\255\000\000\000\000\040\255\
\027\255\000\000\037\255\046\255\033\255\043\255\011\255\000\000\
\049\255\051\255\045\255\047\255\027\255\000\000\008\255\008\255\
\000\000\008\255\000\000\039\255\039\255\000\000\039\255\027\255\
\000\000\050\255\048\255\000\000\052\255\053\255\008\255\000\000\
\039\255\054\255\000\000\000\000\039\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\248\255\247\255\000\000\000\000\030\000\000\000\048\000\000\000\
\000\000\026\000\235\255"

let yytablesize = 275
let yytable = "\029\000\
\008\000\020\000\019\000\038\000\001\000\002\000\004\000\018\000\
\004\000\005\000\035\000\004\000\005\000\006\000\007\000\004\000\
\005\000\008\000\027\000\009\000\008\000\028\000\009\000\030\000\
\008\000\033\000\038\000\016\000\017\000\034\000\049\000\050\000\
\031\000\051\000\052\000\053\000\020\000\054\000\036\000\016\000\
\017\000\039\000\037\000\041\000\018\000\019\000\059\000\060\000\
\020\000\040\000\021\000\062\000\042\000\044\000\045\000\046\000\
\047\000\055\000\056\000\057\000\043\000\032\000\048\000\058\000\
\000\000\000\000\061\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\020\000\019\000\000\000\000\000\
\008\000\020\000\019\000\008\000\020\000\019\000\004\000\018\000\
\000\000\004\000\018\000"

let yycheck = "\008\000\
\000\000\000\000\000\000\025\000\001\000\002\000\000\000\000\000\
\001\001\002\001\020\000\001\001\002\001\006\001\007\001\001\001\
\002\001\010\001\002\001\012\001\010\001\002\001\012\001\001\001\
\010\001\002\001\048\000\001\001\002\001\002\001\039\000\040\000\
\003\001\042\000\044\000\045\000\010\001\047\000\001\001\001\001\
\002\001\005\001\003\001\011\001\006\001\007\001\055\000\057\000\
\010\001\004\001\012\001\061\000\010\001\005\001\004\001\011\001\
\010\001\008\001\011\001\008\001\031\000\014\000\037\000\011\001\
\255\255\255\255\013\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\003\001\003\001\255\255\255\255\
\008\001\008\001\008\001\011\001\011\001\011\001\008\001\008\001\
\255\255\011\001\011\001"

let yynames_const = "\
  PLUS\000\
  EQUALS\000\
  DOT\000\
  LAMBDA\000\
  LET\000\
  IN\000\
  EOF\000\
  QUOTE\000\
  LPAREN\000\
  RPAREN\000\
  CALL\000\
  THEN\000\
  "

let yynames_block = "\
  STRING\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.source_exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.source_exp) in
    Obj.repr(
# 18 "parser.mly"
                                        ( SE_Let(_2,_4,_6) )
# 199 "parser.ml"
               : Ast.source_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's_lexp) in
    Obj.repr(
# 19 "parser.mly"
                                        ( _1 )
# 206 "parser.ml"
               : Ast.source_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.source_exp) in
    Obj.repr(
# 21 "parser.mly"
                                       ( SE_Lam (_2,_4) )
# 214 "parser.ml"
               : 's_lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's_cexp) in
    Obj.repr(
# 22 "parser.mly"
                                        ( _1 )
# 221 "parser.ml"
               : 's_lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_cexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_bexp) in
    Obj.repr(
# 24 "parser.mly"
                                        ( SE_Concat(_1,_3) )
# 229 "parser.ml"
               : 's_cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's_bexp) in
    Obj.repr(
# 25 "parser.mly"
                                        ( _1 )
# 236 "parser.ml"
               : 's_cexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.source_exp) in
    Obj.repr(
# 27 "parser.mly"
                                         (SE_Call (_2, _4) )
# 244 "parser.ml"
               : 's_bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's_appexp) in
    Obj.repr(
# 28 "parser.mly"
                                        ( _1 )
# 251 "parser.ml"
               : 's_bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 's_appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's_aexp) in
    Obj.repr(
# 30 "parser.mly"
                                        ( SE_App(_1,_2) )
# 259 "parser.ml"
               : 's_appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's_aexp) in
    Obj.repr(
# 31 "parser.mly"
                                        ( _1 )
# 266 "parser.ml"
               : 's_appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
                                        ( SE_Var _1 )
# 273 "parser.ml"
               : 's_aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
                                        ( SE_Str _1 )
# 280 "parser.ml"
               : 's_aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.source_exp) in
    Obj.repr(
# 35 "parser.mly"
                                        ( _2 )
# 287 "parser.ml"
               : 's_aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.target_exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.target_exp) in
    Obj.repr(
# 38 "parser.mly"
                                        ( TE_Let(_2,_4,_6) )
# 296 "parser.ml"
               : Ast.target_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_lexp) in
    Obj.repr(
# 39 "parser.mly"
                                        ( _1 )
# 303 "parser.ml"
               : Ast.target_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.target_exp) in
    Obj.repr(
# 41 "parser.mly"
                                       ( TE_Lam (_2,_4) )
# 311 "parser.ml"
               : 't_lexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Ast.target_exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.target_exp) in
    Obj.repr(
# 43 "parser.mly"
                                        ( TE_Call (_2, _4, _7) )
# 320 "parser.ml"
               : 't_lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_cexp) in
    Obj.repr(
# 44 "parser.mly"
                                        ( _1 )
# 327 "parser.ml"
               : 't_lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 't_cexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't_appexp) in
    Obj.repr(
# 46 "parser.mly"
                                        ( TE_Concat(_1,_3) )
# 335 "parser.ml"
               : 't_cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_appexp) in
    Obj.repr(
# 47 "parser.mly"
                                        ( _1 )
# 342 "parser.ml"
               : 't_cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_aexp) in
    Obj.repr(
# 49 "parser.mly"
                                        ( TE_App(_1,_2) )
# 350 "parser.ml"
               : 't_appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_aexp) in
    Obj.repr(
# 50 "parser.mly"
                                        ( _1 )
# 357 "parser.ml"
               : 't_appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                                        ( TE_Var _1 )
# 364 "parser.ml"
               : 't_aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                        ( TE_Str _1 )
# 371 "parser.ml"
               : 't_aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.target_exp) in
    Obj.repr(
# 54 "parser.mly"
                                        ( _2 )
# 378 "parser.ml"
               : 't_aexp))
(* Entry s_exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry t_exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s_exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.source_exp)
let t_exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ast.target_exp)
