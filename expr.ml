(* 
			 CS 51 Final Project
			MiniML -- Expressions
			     Spring 2017
*)

(* Abstract syntax of MiniML expressions *)

open Printf

type unop =
  | Negate    
;;
    
type binop =
  | Plus        (* "+" *)
  | Minus       (* "-" *)
  | Times       (* "*" *)   
  | Equals      (* "=" *)
  | LessThan    (* "<" *)
;;
      
type expr =
  | Var of varid                         (* variables - already a string *)
  | Num of int                           (* integers - inttostring *)
  | Bool of bool                         (* booleans - "true" "false" *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(* Sets of varids *)
module SS = Set.Make (struct
		       type t = varid
		       let compare = String.compare
		     end ) ;;

type varidset = SS.t ;;

(* Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(* Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list;;

(* Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr)  : varidset =
 let filtervars exp v = SS.filter (fun x -> not (x = v)) (free_vars exp) in 
 match exp with 
  | Var v              -> SS.singleton v
  | Unop (_, e)        -> free_vars e 
  | Fun (v, e)         -> filtervars e v
  | Let (v, e1, e2)    -> SS.union (free_vars e1) (filtervars e2 v)
  | Letrec (v, e1, e2) -> SS.union (filtervars e1 v) (filtervars e2 v)
  | Binop (_, e1, e2) | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> 
      SS.union (free_vars e1) (SS.union (free_vars e2) (free_vars e3))
  | _ -> SS.empty 
;;
  
(* returns true if a var is the set of free_vars *)
let is_free_var var exp: bool =
  SS.exists (fun x -> x = var) (free_vars exp)
;; 

(* Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let gensymcnt = ref 0
;; 

let inc (i : int ref) : unit =
  i := !i + 1
;;

let new_varname () : varid =
  let x = !gensymcnt in (inc gensymcnt);
    string_of_int x
;;

(* Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let sub = subst var_name repl in  
  match exp with 
  | Var v                    -> if v = var_name then repl else exp
  | Unop (n, e)              -> Unop (n, sub e)
  | Binop (b, e1, e2)        -> Binop (b, sub e1, sub e2)
  | Conditional (e1, e2, e3) -> Conditional (sub e1, sub e2, sub e3)
  | App (e1, e2) -> 
    (match e1 with
      | Fun (v, e) -> subst v (sub e2) (sub e)
      | _          -> App (sub e1, sub e2))
  | Fun (v, e) -> 
    if v = var_name then exp else 
      if is_free_var v repl then let new_var = new_varname() in 
        Fun (new_var, sub (subst v (Var (new_var)) e))
      else Fun (v, sub e)
  | Let (v, e1, e2) -> 
    if v = var_name then Let (v, sub e1, e2) else 
      if is_free_var v repl then let new_var = new_varname() in
        Let (new_var, sub e1, sub (subst v (Var new_var) e2))
      else Let (v, sub e1, sub e2)
  | Letrec (v, e1, e2) -> 
    if v = var_name then Letrec (v, sub e1, e2) else 
    if is_free_var v repl then let new_var = new_varname() in
      (Letrec (new_var, sub e1, sub (subst v (Var new_var) e2)))
    else (Letrec (v, sub e1, sub e2))
  | x -> x
;;


(* Returns a string representation of the Binop operator *)
let bin_str (b : binop) : string =
  match b with 
  | Plus     -> "Plus" 
  | Minus    -> "Minus" 
  | Times    -> "Times" 
  | Equals   -> "Equals"
  | LessThan -> "LessThan"

(* Returns an algebraic symbol representation of the Binop operator as a string*)
let binop_to_symbol (b : binop) : string =
  match b with 
  | Plus     -> "+" 
  | Minus    -> "-" 
  | Times    -> "*" 
  | Equals   -> "="
  | LessThan -> "<"

(* exp_to_abstract_string -- Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
  let s = exp_to_string in 
  match exp with
  | Var v              -> v
  | Num i              -> string_of_int i
  | Bool b             -> if b then "true" else "false"
  | Unop (_, e)        -> sprintf "-%s" (s e)
  | Binop (b, e1, e2)  -> (s e1) ^ (binop_to_symbol b) ^ (s e2)
  | Fun (v, e)         -> sprintf "fun %s -> %s" v (s e)
  | Let (v, e1, e2)    -> sprintf "let %s = %s in %s" v (s e1) (s e2)
  | Letrec (v, e1, e2) -> sprintf "let rec %s = %s in %s" v (s e1) (s e2)
  | Raise              -> "Raise"
  | Unassigned         -> "Unassigned"
  | App (e1, e2)       -> (s e1) ^ (s e2)
  | Conditional (e1, e2, e3) -> 
                          sprintf "if %s then %s else %s" (s e1) (s e2) (s e3)
;;

(* exp_to_abstract_string: Returns a string representation of the abstract
   syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  let s = exp_to_abstract_string in 
  match exp with 
  | Var v              -> sprintf "Var(%s)" v
  | Num i              -> sprintf "Num(%s)" (string_of_int i)
  | Bool b             -> if b then "Bool(true)" else "Bool(false)"
  | Unop (_, e)        -> sprintf "Unop(Negate, %s)" (s e)
  | Binop (b, e1, e2)  -> sprintf "Binop(%s, %s, %s)" (bin_str b) (s e1) (s e2)
  | Fun (v, e)         -> sprintf "Fun(%s, %s)" v (s e)
  | Let (v, e1, e2)    -> sprintf "Let(%s, %s, %s)" v (s e1) (s e2)
  | Letrec (v, e1, e2) -> sprintf "Letrec(%s, %s, %s)" v (s e1) (s e2)
  | Raise              -> "Raise"
  | Unassigned         -> "Unassigned" 
  | App (e1, e2)       -> sprintf "App(%s, %s)" (s e1) (s e2)
  | Conditional (e1, e2, e3) 
                       -> sprintf "Conditional(%s, %s, %s)" (s e1) (s e2) (s e3)
  ;;
