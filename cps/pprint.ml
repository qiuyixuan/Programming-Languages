open Ast
open Format
  
let ident v = printf "%s" v
let printStr s = printf "\"%s\"" s
let printLam p x e = printf "@[<2>(\\%s.@ %a)@]" x p e
let printApp p l r = printf "@[<2>(%a@ %a)@]" p l p r
let printLet p x e1 e2 = printf "@[let %s@ =@ %a@ in@ %a@]" x p e1 p e2
    
let rec printTargExp' ppf = function
  | TE_Var v -> ident v
  | TE_Str s -> printStr s
  | TE_Concat (e1,e2) -> printf "@[<2>%a@ + %a@]" printTargExp' e1 printTargExp' e2
  | TE_Lam (x,e) -> printLam printTargExp' x e
  | TE_App (l,r) -> printApp printTargExp' l r
  | TE_Let (x,e1,e2) -> printLet printTargExp' x e1 e2
  | TE_Call (op, arg, cont) ->
      printf "@[<2>call@ \"%s\"(%a) then@ %a@]" op printTargExp' arg printTargExp' cont
	
and printSrcExp' ppf = function
  | SE_Var v -> ident v
  | SE_Str s -> printStr ("\"" ^ s ^ "\"")
  | SE_Concat (e1,e2) -> printf "@[<2>%a@ + %a@]" printSrcExp' e1 printSrcExp' e2
  | SE_Lam (x,e) -> printLam printSrcExp' x e
  | SE_App (l,r) -> printApp printSrcExp' l r
  | SE_Let(x,e1,e2) -> printLet printSrcExp' x e1 e2
  | SE_Call (op, arg) ->
      printf "@[<2>call@ \"%s\"(%a)@]" op printSrcExp' arg
	
let printSrcExp e = 
  printSrcExp' Format.std_formatter e
    
let printTargExp e = 
  printTargExp' Format.std_formatter e
    
