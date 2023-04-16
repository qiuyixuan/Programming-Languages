(* variables *)
type var = string

module VarMap = Map.Make(struct
  type t = var
  let compare = Stdlib.compare
end)

type target_exp =
  | TE_Var of var
  | TE_Str of string
  | TE_Concat of target_exp * target_exp
  | TE_Lam of var * target_exp
  | TE_App of target_exp * target_exp
  | TE_Let of var * target_exp * target_exp
  | TE_Call of string * target_exp * target_exp (* Call(op, arg, continuation) *)
and env = target_exp VarMap.t

type source_exp =
  | SE_Var of var
  | SE_Str of string
  | SE_Concat of source_exp * source_exp
  | SE_Lam of var * source_exp
  | SE_App of source_exp * source_exp
  | SE_Let of var * source_exp * source_exp
  | SE_Call of string * source_exp              (* Call(op, arg) *)

let var_cell = ref 0
let freshvar () =
  let x = !var_cell in 
  let c = Char.chr (x mod 26 + 97) in 
  let n = x / 26 in 
  let s = "'" ^ String.make 1 c in 
  incr var_cell;
  if n = 0 then s else s ^ "_" ^ string_of_int n 
