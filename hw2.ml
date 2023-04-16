(*
 *  CS 152 Programming Languages
 *
 *  Homework 2 Question 4
 *
 *
 *
 *  Instructions
 *
 *  Please fill in the placeholder functions per the written assignment
 *  Please do not change the signature of the functions
 *  Make sure that this file loads without any errors in a
 *     clean OCaml shell
 *
 *)



(*************************************************************

   Type for lambda terms

 ************************************************************)



type lterm = 
    LId of string
  | LLam of string * lterm
  | LApp of lterm * lterm



(*************************************************************

   Module implementing some helper functions
   
   LambdaParser.parse : string -> lterm
   LambdaParser.pp : lterm -> string

 ************************************************************)

module LambdaParser = struct

  let lexer = Genlex.make_lexer ["(";")";".";"/"]

  let lex s = 
    let str = lexer (Stream.of_string s)  in
    let rec loop () = 
      match (Stream.peek str) with
      | None -> []
      | Some _ -> let elt = Stream.next str in elt::(loop())  in
    loop ()

  let expect elt cs = 
    match cs with
    | f::cs when f = elt -> Some cs
    | _ -> None

  let expect_ident cs = 
    match cs with
    | (Genlex.Ident id)::cs -> Some (id,cs)
    | _ -> None

  let rec parse_term cs = 
    match parse_ident_terms cs with
    | Some x -> Some x
    | None -> 
	(match parse_lambda cs with
	|	Some x -> Some x
	|	None ->
	    (match parse_group_terms cs with
	    | Some x -> Some x
	    | None -> 
		(match parse_ident cs with
		|	Some x -> Some x
		|	None -> 
		    (match parse_group cs with
		    | Some x -> Some x
		    | None -> None))))

  and parse_ident_term cs = 
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) -> 
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp(term1,term2),cs))

  and parse_ident_terms cs =    (* ident term+ *)
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) -> 
	(match parse_terms cs with
	|	None -> None
	|	Some (f,cs) -> Some (f term1,cs))

  and parse_group_terms cs =    (* group term+ *)
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_terms cs with
	|	None -> None
	|	Some (f,cs) -> Some (f term1, cs))

  and parse_terms cs = 
    match parse_ident cs with
    | Some (term1,cs) -> 
	(match parse_terms cs with
	|	None -> Some ((fun t -> LApp(t,term1)),cs)
	|	Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
    | None-> 
	(match parse_group cs with
	|	Some (term1,cs) -> 
	    (match parse_terms cs with
	    | None -> Some ((fun t -> LApp(t,term1)),cs)
	    | Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
	|	None -> None)
	  

  and parse_ident cs =
    match expect_ident cs with
    | None -> None
    | Some (id,cs) -> Some (LId id,cs)

  and parse_lambda cs = 
    match expect (Genlex.Kwd "/") cs with
    | None -> None
    | Some cs -> 
	(match expect_ident cs with
	|	None -> None
	|	Some (id,cs) -> 
	    (match expect (Genlex.Kwd ".") cs with
	    | None -> None
	    | Some cs -> 
		(match parse_term cs with
		|	None -> None
		|	Some (term,cs) -> Some (LLam (id,term),cs))))

  and parse_group_term cs =
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp (term1,term2),cs))

  and parse_group cs =
    match expect (Genlex.Kwd "(") cs with
    | None -> None
    | Some cs ->
	(match parse_term cs with
	|	None -> None
	|	Some (term,cs) ->
	    (match expect (Genlex.Kwd ")") cs with
	    | None -> None
	    | Some cs -> Some (term,cs)))

  let parse str = 
    match parse_term (lex str) with
    | Some (term,[]) -> term
    | _ -> failwith ("Cannot parse "^str)

  let rec pp term = 
    match term with
    | LId x -> x
    | LLam (x,t) -> "/"^x^"."^(pp t)
    | LApp (t1,t2) -> 
	let t1' = (match t1 with
	| LLam _ -> "("^(pp t1)^")"
	| _ -> pp t1)  in
	let t2' = (match t2 with
	| LApp _ -> "("^(pp t2)^")"
	| LLam _ -> "("^(pp t2)^")"
	| _ -> pp t2)  in
	t1'^" "^t2'
end
    


(* PART A *)

let rec free_vars (e:lterm):string list = 
  match e with
  | LId x -> [x]
  | LLam (x, t) -> List.filter(fun y -> y <> x) (free_vars t)
  | LApp (t1, t2) -> List.append (free_vars t1) (free_vars t2)

let make_new_var (lst: string list) : string =
  String.concat " " lst

let rec substitute (m:lterm) (s:string) (n:lterm):lterm =
  match m with
  | LId y -> if s = y then n else m
  | LApp (t1, t2) -> LApp (substitute t1 s n, substitute t2 s n)
  | LLam (y, t) -> 
      if s = y then LLam (y, t) 
      else 
        if List.mem y (free_vars n) then 
          let all_vars = free_vars n @ free_vars t @ [s] in
          let z = make_new_var all_vars in 
          LLam (z, substitute (substitute t y (LId z)) s n)
        else LLam (y, substitute t s n)



(* PART B *)

let rec reduce (t:lterm):lterm option =
  match t with
  | LApp (LLam (x, t1), t2) -> Some (substitute t1 x t2)
  | LApp (t1, t2) ->
    (match reduce t1 with
    | Some t1' -> Some (LApp (t1', t2))
    | None -> 
      (match reduce t2 with
        | Some t2' -> Some (LApp (t1, t2'))
        | None -> None))
  | LLam (x, t) ->
    (match reduce t with
    | Some t' -> Some (LLam (x, t'))
    | None -> None)
  | _ -> None
  


(* PART C *)

let rec normal_form (t:lterm):lterm =
  match reduce t with
  | Some t' -> normal_form t'
  | None -> t


let eval (input:string):string =
  let term = LambdaParser.parse input in
  LambdaParser.pp (normal_form term)



(* PART D *)

type abbs = (string * lterm) list

(* sample encodings *)

let encs = let p = LambdaParser.parse in
           [ ("true", p "/x./y.x");
	     ("false", p "/x./y.y");
	     ("if", p "/c./x./y.c x y");
	     ("and", p "/b./c.b c false");
	     ("or", p "/b./c.b true c");
	     ("not", p "/b.b false true");
	     ("_0", p "/f./x.x");
	     ("_1", p "/f./x.(f x)");
	     ("_2", p "/f./x.(f (f x))");
	     ("_3", p "/f./x.(f (f (f x)))");
	     ("_4", p "/f./x.(f (f (f (f x))))");
	     ("_5", p "/f./x.(f (f (f (f (f x )))))");
	     ("succ", p "/n./f./x.f (n f x)");
	     ("plus", p "/m./n./f./x.(m f) (n f x)");
	     ("times", p "/m./n./f./x.m (n f) x");
	     ("iszero", p "/n.n (/x.false) true");
	     ("pred", p "/n./f./x.n (/g.(/h.h (g f))) (/u.x) (/u.u)");
	     ("Y", p "/f.(/x.f (x x)) (/x.f (x x))");
	     ("fact", p "Y (/fact./n.(iszero n) _1 (times n (fact (pred n))))") ]
  
let rec helper (abbs:abbs) (l:lterm):lterm =
  match abbs with
  | (x, lval)::t -> substitute (helper t l) x lval
  | [] -> l
       
let normal_form_abbs (abbs:abbs) (l:lterm):lterm =
  normal_form (helper abbs l)

let eval_abbs (abbs:abbs) (input:string):string =
  let term = LambdaParser.parse input in
  LambdaParser.pp (normal_form_abbs abbs term)
