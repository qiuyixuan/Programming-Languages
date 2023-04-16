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

val s_exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.source_exp
val t_exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.target_exp
