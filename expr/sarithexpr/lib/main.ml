open Ast
exception TypeError of string;;
let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | And(e1, e2) -> "And(" ^ (string_of_expr e1) ^","^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_expr e1) ^","^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not " ^(string_of_expr e)
  | Succ(e) -> "Succ " ^(string_of_expr e)
  | Pred(e) -> "Pred " ^(string_of_expr e)
  | IsZero(e) -> "IsZero " ^(string_of_expr e)

let string_of_type = function 
  BoolT -> "BoolT"
  | NatT -> "NatT"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec typecheck : expr -> exprtype = function
  | True -> BoolT
  | False -> BoolT
  | Not(e) -> (match typecheck e with 
  | BoolT -> BoolT
  | _ -> raise(TypeError (string_of_expr e ^ " has type Nat, but type Bool is expected")))
  | And(e0, e1) -> (match (typecheck e0, typecheck e1) with 
  | (BoolT, BoolT) -> BoolT
  | _ -> raise (TypeError "error"))
  | Or(e0, e1) -> (match (typecheck e0, typecheck e1) with 
  | (BoolT, BoolT) -> BoolT
  | _ -> raise (TypeError "error"))
  | If(e0, e1, e2) -> (match (typecheck e0, typecheck e1, typecheck e2) with 
  | (BoolT, then1,else1) -> if then1 = else1 then else1 else raise (TypeError "error")
  | _ -> raise (TypeError "error"))
  | Zero -> NatT
  | Succ e-> (match typecheck e with 
  | NatT -> NatT
  | _ -> raise (TypeError "error"))
  | Pred e-> (match typecheck e with 
  | NatT -> NatT
  | _ -> raise (TypeError "error"))
  | IsZero e-> (match typecheck e with 
  | NatT -> BoolT
  | _ -> raise (TypeError "error"))

  




let rec is_nv = function 
  | Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

let rec trace1 = function
    If(True,e1, _) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If(trace1 e0, e1, e2)
  | Not True -> False
  | Not False -> True
  | Not e0 -> Not (trace1 e0)
  | And (True, True) -> True
  | And (True, e0) -> And(True, trace1 e0)
  | And (False, _) -> False
  | And (e0, e1) -> And(trace1 e0, trace1 e1)
  | Or (True, _) -> True
  | Or (False, e0) -> Or(False, trace1 e0)
  | Or (e0, e1) -> Or(trace1 e0, trace1 e1)
  | Succ (e) -> Succ(trace1 e)
  | Pred(Succ(e)) -> e
  | Pred (e) -> Pred(trace1 e)
  | IsZero (Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval:expr -> exprval = function
    True -> (Bool true)
  | False -> (Bool false)
  | If(e0, e1,e2)->
    (match eval e0 with 
      (Bool true) -> eval e1
    | (Bool false) ->  eval e2 
    | _ -> raise NoRuleApplies)
  | And(e1, e2) ->
   ( match (eval e1, eval e2) with
    (Bool e1, Bool e2) -> Bool (e1 && e2)
    | _ -> raise NoRuleApplies)
    |Or(e1, e2) ->
      (match (eval e1, eval e2) with
      (Bool e1, Bool e2) -> Bool (e1 || e2)
      | _ -> raise NoRuleApplies)
  | Not(e) ->
    (match eval e with 
    Bool b -> Bool (not b)
    | _ -> raise NoRuleApplies)
  | Zero -> Nat 0
  |Succ (e) ->
      (match eval e with
      Nat n1 -> Nat(n1+1)
     | _ -> raise NoRuleApplies)
  | Pred (e) -> 
        (match eval e with
        Nat n1 when n1 > 0 -> Nat(n1-1)
        |_-> raise NoRuleApplies)
    | IsZero (e) -> 
          (match eval e with 
          Nat n -> Bool(n=0)
         | _ -> raise NoRuleApplies
          )
