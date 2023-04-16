open Ast
open Pprint
open Format
open Server
  
exception IllformedExpression of string
    
let id_cont : target_exp =
  let x = freshvar () in
  TE_Lam (x, TE_Var x)
    
(* check if x is free in e *)
let rec is_free (e:target_exp) (x:var) : bool =
  match e with 
  | TE_Var y -> (y = x)
  | TE_Str s -> false
  | TE_Concat (e1, e2) -> (is_free e1 x) || (is_free e2 x)
  | TE_Lam (y, e1) -> 
      if (y = x) then false else is_free e1 x
  | TE_App (e1, e2) -> (is_free e1 x) || (is_free e2 x)
  | TE_Let (y, e1, e2) -> 
      (is_free e1 x) || (if (y = x) then false else is_free e2 x)      
  | TE_Call (op, arg, cont) -> (is_free arg x) || (is_free cont x)
	
	
(* Return a variable that is free in neither e1 or e2 and different from y *)
let newVar(e1:target_exp) (e2:target_exp) (y:var):var =
  let rec newVar' (e1:target_exp) (e2:target_exp) (x:var):var =
    if (not(is_free e1 x) && not(is_free e2 x)) then 
      x 
    else 
      newVar' e1 e2 (x ^ "'") 
  in
  newVar' e1 e2 (y ^ "'")
    

let rec subst (e : target_exp) (e' : target_exp) (x : var) : target_exp =
  match e with 
  | TE_Var y -> if y = x then e' else TE_Var y
  | TE_Str s -> TE_Str s
  | TE_Concat (e1, e2) ->
      TE_Concat ((subst e1 e' x), (subst e2 e' x))
  | TE_Lam (y, e1) -> 
      if (y = x) then 
	TE_Lam (y, e1)
      else if not(is_free e' y) then 
	TE_Lam(y, subst e1 e' x) 
      else 
	let z = (newVar e1 e' y) in
	let e1' = (subst e1 (TE_Var(z)) y) in
	TE_Lam(z, subst e1' e' x)
  | TE_App (e1, e2) ->
      TE_App ((subst e1 e' x), (subst e2 e' x))
  | TE_Let (y, e1, e2) -> 
      if (y = x) then 
	TE_Let (y, subst e1 e' x, e2)
      else if not(is_free e' y) then 
	TE_Let(y, subst e1 e' x, subst e2 e' x) 
      else 
	let z = (newVar e2 e' y) in
	let e2' = (subst e2 (TE_Var(z)) y) in
	TE_Let(z, subst e1 e' x, subst e2' e' x)      
  | TE_Call (op, arg, cont) ->
      TE_Call (op, (subst arg e' x), (subst cont e' x))
	

let rec eval (code : target_exp) : target_exp =
  match code with
  | TE_Var x -> raise (IllformedExpression "Shouldn't have variables when executing")
  | TE_Str s -> TE_Str s
  | TE_Concat (e1, e2) -> (
      let e1' = eval e1 in
      let e2' = eval e2 in
      match (e1', e2') with
      | (TE_Str s1, TE_Str s2) -> TE_Str (s1^s2)
      | _ -> raise (IllformedExpression "Trying to concatenate non-strings"))
  | TE_Lam (x, e) -> TE_Lam (x, e)
  | TE_App (e1, e2) -> (
    match eval e1 with
    | TE_Lam (x,eb) -> 
	eval (subst eb (eval e2) x)
    | _ -> raise (IllformedExpression "Trying to applying a non-function"))
  | TE_Let (x, e1, e2) ->
    eval (subst e2 (eval e1) x)
  | TE_Call (op, arg, cont) ->
      let serverResult = 
	(match (eval arg) with 
	| TE_Str opArg -> Server.do_server_op op opArg
	| _ -> raise (IllformedExpression "Server call argument is not a string")) in
      eval (TE_App (cont, TE_Str serverResult))
	
