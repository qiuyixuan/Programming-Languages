open Ast

let rec cpsTranslate (src : source_exp) : target_exp =
  let k = freshvar () in
  let res = match src with
  | SE_Var x -> 
    TE_App(TE_Var k, TE_Var x)
  | SE_Str s -> 
    TE_App (TE_Var k, TE_Str s)
  | SE_Lam (x, e) ->
    let k' = freshvar () in
    TE_App(TE_Var k, TE_Lam (x, TE_Lam (k', TE_App (cpsTranslate e, TE_Var k'))))
  | SE_Concat (e1, e2) ->
    let a = freshvar () in
    let b = freshvar () in
    TE_App (cpsTranslate e1, TE_Lam (a, TE_App (cpsTranslate e2,
        TE_Lam (b, TE_App (TE_Var k, TE_Concat (TE_Var a, TE_Var b))))))
  | SE_App (e1, e2) ->
    let f = freshvar () in
    let v = freshvar () in
    TE_App (cpsTranslate e1, TE_Lam (f, TE_App (cpsTranslate e2,
        TE_Lam (v, TE_App (TE_App (TE_Var f, TE_Var v), TE_Var k)))))
  | SE_Let (x, e1, e2) ->
    TE_App (cpsTranslate e1, TE_Lam (x, TE_App (cpsTranslate e2, TE_Var k)))
  | SE_Call (s, e) ->
    let v = freshvar () in
    TE_App (cpsTranslate e, TE_Lam (v, TE_Call (s, TE_Var v, TE_Var k)))
  in TE_Lam (k, res)
