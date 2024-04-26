open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*
type values = Int of int|Bool of bool|String of string
*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

(*
let extend env x v = (x,v)::env
*)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(*
let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x
*)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(**
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)
*)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value(value) -> value
  | ID(id) -> ref_lookup env id
  | Not(expr) -> eval_not env expr
  | Binop(op, expr1, expr2) -> eval_binop env op expr1 expr2
  | If(expr1, expr2, expr3) -> eval_if env expr1 expr2 expr3
  | Let(var, b, expr1, expr2) -> eval_let env var b expr1 expr2
  | Fun(var, expr) -> eval_fun env var expr
  | FunctionCall(expr1, expr2) -> eval_function_call env expr1 expr2

and eval_not env e =
  let v = eval_expr env e in
  match v with
  | Bool(b) -> Bool(not b)
  | _ -> raise (TypeError "Expected type bool")

and eval_binop env op e1 e2 =
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in 
  match op,v1,v2 with
  | Add,Int(x1),Int(x2) -> Int(x1 + x2)
  | Add,_,_ -> raise (TypeError "Expected type int")
  | Sub,Int(x1),Int(x2) -> Int(x1 - x2)
  | Sub,_,_ -> raise (TypeError "Expected type int")
  | Mult,Int(x1),Int(x2) -> Int(x1 * x2)
  | Mult,_,_ -> raise (TypeError "Expected type int")
  | Div,Int(x1),Int(x2) -> if x2 = 0 then raise DivByZeroError else Int(x1 / x2)
  | Div,_,_ -> raise (TypeError "Expected type int")
  | Concat,String(x1),String(x2) -> String(x1 ^ x2)
  | Concat,_,_ -> raise (TypeError "Expected type string")
  | Greater,Int(x1),Int(x2) -> Bool(x1 > x2)
  | Greater,_,_ -> raise (TypeError "Expected type int")
  | Less,Int(x1),Int(x2) -> Bool(x1 < x2)
  | Less,_,_ -> raise (TypeError "Expected type int")
  | GreaterEqual,Int(x1),Int(x2) -> Bool(x1 >= x2)
  | GreaterEqual,_,_ -> raise (TypeError "Expected type int")
  | LessEqual,Int(x1),Int(x2) -> Bool(x1 <= x2)
  | LessEqual,_,_ -> raise (TypeError "Expected type int")
  | Equal,Int(x1),Int(x2) -> Bool(x1 = x2)
  | Equal,Bool(x1),Bool(x2) -> Bool(x1 = x2)
  | Equal,String(x1),String(x2) -> Bool(x1 = x2)
  | Equal,_,_ -> raise (TypeError "Cannot compare types")
  | NotEqual,Int(x1),Int(x2) -> Bool(x1 <> x2)
  | NotEqual,Bool(x1),Bool(x2) -> Bool(x1 <> x2)
  | NotEqual,String(x1),String(x2) -> Bool(x1 <> x2)
  | NotEqual,_,_ -> raise (TypeError "Cannot compare types")
  | Or,Bool(x1),Bool(x2) -> Bool(x1 || x2)
  | Or,_,_ -> raise (TypeError "Expected type bool")
  | And,Bool(x1),Bool(x2) -> Bool(x1 && x2)
  | And,_,_ -> raise (TypeError "Expected type bool")

and eval_if env e1 e2 e3 = 
  let v1 = eval_expr env e1 in 
  match v1 with 
  | Bool(b) -> if b then eval_expr env e2 else eval_expr env e3
  | _ -> raise (TypeError "Expected type bool")

and eval_let env id r e1 e2 = (* UNFINISHED *)
  if r then (* its recursive *) 
    (* NOT SURE OF THIS CASE WORKS OR IF FUNCTIONCALL DOESNT WORK *)
    let env' = ref_extend_tmp env id in
    let v1 = eval_expr env' e1 in
    let tmp = ref_update env' id v1 in
    eval_expr env' e2
  else (* its not recursive*)
    (* PRETTY SURE THIS ONE WORKS *)
    let v1 = eval_expr env e1 in 
    let env' = ref_extend env id v1 in 
    eval_expr env' e2
    
and eval_fun env str e = 
  Closure(env, str, e)

and eval_function_call env e1 e2 =
  let closure = eval_expr env e1 in
  let v = eval_expr env e2 in
  match closure with 
  | Closure(a,x,e) -> 
    let a' = ref_extend a x v in (* based on opsem pdf, i think we need to extend a to contain x:v*)
    eval_expr a' e 
  | _ -> raise (TypeError "Not a function")

  

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(var,expr) -> 
    let env' = ref_extend_tmp env var in
    let v = eval_expr env' expr in
    let tmp = ref_update env' var v in
    (env', Some (v))
  | Expr(expr) -> (env, Some(eval_expr env expr))
  | NoOp -> (env, None)
