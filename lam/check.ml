open Ast
open Helper
open Pprint

exception TypeError 
exception UnificationError
exception UnimplementedError

(* [unify c0] solves [c0] (if possible), yielding a substitution. Raise UnificationError if constraints cannot be unified. *)
let rec unify (c0:constr) : subst = 
  if Constr.is_empty c0 then VarMap.empty
  else
    let (t, t') = Constr.choose c0 in
    let c = Constr.remove (t, t') c0 in
      if typ_eq t t' then unify c
      else 
        match (t, t') with
        | (TVar x, _) ->
          if (VarSet.mem x (ftvs t')) then raise UnificationError
          else
            let sigma = unify (subst_constr c x t') in
            VarMap.add x t' sigma
        | (_, TVar x) ->
          if (VarSet.mem x (ftvs t)) then raise UnificationError
          else
            let sigma = unify (subst_constr c x t) in
            VarMap.add x t sigma
        | (TArrow (t0, t1), TArrow (t0', t1')) ->
          unify (Constr.add (t0, t0') (Constr.add (t1, t1') c))
        | (TPair (t0, t1), TPair (t0', t1')) ->
          unify (Constr.add (t0, t0') (Constr.add (t1, t1') c))
        | _ -> raise UnificationError

(* [check g e0] typechecks [e0] in the context [g] generating a type and a set of constraints. Raise TypeError if constraints cannot be generated. *)
let rec check (g:context) (e0:exp) : typ * constr = 
  match e0 with
    | True -> (TBool, Constr.empty)
    | False -> (TBool, Constr.empty)
    | Int _ -> (TInt, Constr.empty)
    | Var x -> (VarMap.find x g, Constr.empty)
    | App (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      let y = next_tvar () in
      (y, Constr.add (t1, TArrow(t2, y)) (Constr.union c1 c2))
    | Lam (x, e) ->
      let t1 = next_tvar () in
      let (t2, c) = check (VarMap.add x t1 g) e in
      (TArrow(t1, t2), c)
    | Plus (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      (TInt, Constr.add (t1, TInt) (Constr.add (t2, TInt) (Constr.union c1 c2)))
    | Minus (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      (TInt, Constr.add (t1, TInt) (Constr.add (t2, TInt) (Constr.union c1 c2)))
    | Times (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      (TInt, Constr.add (t1, TInt) (Constr.add (t2, TInt) (Constr.union c1 c2)))
    | Pair (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      (TPair(t1, t2), Constr.union c1 c2)
    | Fst e ->
      let (t, c) = check g e in
      let x = next_tvar () in
      let y = next_tvar () in
      (x, Constr.add (t, TPair(x, y)) c)
    | Snd e ->
      let (t, c) = check g e in
      let x = next_tvar () in
      let y = next_tvar () in
      (y, Constr.add (t, TPair(x, y)) c)
    | If (e1, e2, e3) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      let (t3, c3) = check g e3 in
      (t2, Constr.add (t2, t3) (Constr.add (t1, TBool) (Constr.union c1 (Constr.union c2 c3))))
    | Eq (e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check g e2 in
      (TBool, Constr.add (t2, TInt) (Constr.add (t1, TInt) (Constr.union c1 c2)))
    | Let (x, e1, e2) ->
      let (t1, c1) = check g e1 in
      let (t2, c2) = check (VarMap.add x t1 g) e2 in
      (t2, Constr.union c1 c2)
    | Letrec (f, x, e1, e2) ->
      let y = next_tvar () in
      let t = next_tvar () in
      let (t1, c1) = check (VarMap.add x t (VarMap.add f y g)) e1 in
      let (t2, c2) = check (VarMap.add f y g) e2 in
      (t2, Constr.add (y, (TArrow(t, t1))) (Constr.union c1 c2))
