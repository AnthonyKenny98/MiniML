(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
                             Spring 2017
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
open Printf;;
  
(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException;;


(* Environments and values *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
    val value_to_expr : value -> expr
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure(exp, env)
   ;;

    (* Looks up the value of a variable in the environment *)
    let rec lookup (env : env) (varname : varid) : value =
      match env with
      | []     -> raise (EvalError ("Unbound Variable: " ^ varname))
      | (var, valref) :: t -> if var = varname then !valref 
                          else lookup t varname;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: env (* (List.filter (fun (x, _) -> not (x = varname)) env) *);;

    let to_string (value_to_string : ?printenvp:bool -> value -> string) env  =
      List.fold_left (fun _ (var, varref) -> sprintf "(%s : %s)" var (value_to_string !varref)) "" env;; 

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> sprintf "Val (%s)" (exp_to_string exp)
      | Closure (exp, env) -> 
          let envp = if printenvp then 
            to_string value_to_string env else "env" in
            sprintf "Closure (%s %s)" (exp_to_string exp) envp;;

   (* Returns a printable string representation of an environment *)
   let env_to_string (env : env) : string =
      to_string value_to_string env;;

   let value_to_expr (value : value) : expr = 
    match value with 
    | Val expr | Closure (expr, _) ->  expr;;

  end
;;
  
(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)


(** The external evaluator, which can be either the identity function,
    the substitution model version or the dynamic or lexical
    environment model version. *)

let vte = Env.value_to_expr;;

let eval_x ?(subst = false) (exp : expr) (env : Env.env) 
            (eval : expr -> Env.env -> Env.value) = 
  match exp with
  | Num _ | Unassigned 
  | Raise | Bool _     -> Env.Val(exp)
  | Unop (unop, e)     -> Env.Val(Unop(unop, (vte (eval e env))))
  | App _              -> eval exp env
  | Let (v, e1, e2)    -> eval e2 (Env.extend env v (ref (eval e1 env)))
  | Letrec (v, e1, e2) -> eval e2 (Env.extend env v 
                (ref (eval e1 (Env.extend env v (ref (Env.Val(Unassigned)))))))
  | Var x -> 
    (match Env.lookup env x with 
    | Val y | Closure (y, _) -> Env.Val(y))  
  | Binop (binop, e1, e2) -> 
    (match (vte (eval e1 env)), (vte (eval e2 env)) with
    | Num x, Num y -> 
      (match binop with
      | Plus     -> Env.Val(Num (x + y))
      | Minus    -> Env.Val(Num (x - y))
      | Times    -> Env.Val(Num (x * y))
      | Equals   -> Env.Val(Bool (x = y))
      | LessThan -> Env.Val(Bool (x < y)))
    | _,_ -> raise (EvalError ("Invalid Binop Expression: " ^ 
            (exp_to_string (Binop(binop, vte (eval e1 env), vte (eval e2 env)))))))
  | Conditional (condition, e1, e2) -> 
      (match vte (eval condition env) with 
      | Bool b -> if b then (eval e1 env) else (eval e2 env) 
      | _ -> raise (EvalError ("Invalid Condition: " ^ (exp_to_string condition)))) 
  | _ -> if subst then Env.Val(exp) else eval exp env

let eval_t exp _env = exp ;;

let rec eval_s exp env : Env.value = 
  let eval_s' e = eval_s e env in
  match exp with 
  | Var x            -> raise (EvalError ("Unbound Variable: " ^ x))
  | Let (v, e1, e2)  -> eval_s' (subst v e1 e2)
  | Letrec (x, v, p) -> eval_s' (subst x (subst x (Letrec(x, v, Var(x))) v) p)
  | App (f, e2) -> 
    (match vte (eval_s' f) with
    | Fun (x, p) -> eval_s' (subst x e2 p) 
    | _      -> raise (EvalError (sprintf 
            "This cannot be applied: %s is not a function" (exp_to_string f))))
  | _ -> eval_x exp env eval_s ~subst:true


let rec eval_d exp env : Env.value =
match exp with 
  | App (f, v) -> 
    (match vte (eval_d f env) with
    | Fun (x, e) -> eval_d e (Env.extend env x (ref (eval_d v env)))
    | _     -> raise (EvalError (sprintf 
            "This cannot be applied: %s is not a function" (exp_to_string f))))
  | Fun _ -> Val(exp)
  | _     -> eval_x exp env eval_d
;;

let rec eval_l exp env : Env.value = 
match exp with 
  | Var x -> 
    (match Env.lookup env x with 
    | Val y -> Env.Val(y)
    | Closure (v, e) -> Env.Closure(v, e))
  | App (f, v) -> 
    (match (eval_l f env) with  
    | Env.Closure (Fun (x, e), env1) -> eval_l e (Env.extend env1 x (ref (eval_d v env1)))
    | _     -> raise (EvalError (sprintf 
            "This cannot be applied: %s is not a function" (exp_to_string f))))
  | Fun (_) -> Env.close exp env 
  | _       -> eval_x exp env eval_l
;;



let evaluate = eval_d ;;
