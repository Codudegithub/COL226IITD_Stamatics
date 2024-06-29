type myBool = T | F ;; 

let myBool2bool b = match b with
  | T -> true | F-> false ;;

let myNot b1 = match b1 with 
    T->F | F->T 
;;

let myAnd b1 b2 = match b1 with
    F->F 
    | T->b2 
;;

let myOr b1 b2 = match b1 with 
    T->T
    | F-> b2 
;;

let print_myBool b =  match b with
    T-> print_endline "T" | F-> print_endline "F"
;;

(* Defining the abstract expression data type *)
type exp = Num of int | Bl of myBool | V of string | Plus of exp*exp | Times of exp*exp | And of exp*exp | Or of exp*exp | Not of exp | Eq of exp*exp | Gt of exp*exp ;;
(* A term is essentially just the 1D representation of a syntax tree :  Abstract Syntax Tree. To help resolve expressions, we define two measure functions *)

(* ht function, leaves are at height 0 *)

let rec ht e =  match e with 
  | Num n -> 0
  | Bl b-> 0
  | V x -> 0
  | Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Times (e1,e2) -> 1+ (max (ht e1) (ht e2))
  | And (e1,e2) -> 1+ (max (ht e1) (ht e2))
  | Or (e1,e2) -> 1+ (max (ht e1) (ht e2))
  | Not e1 -> 1+ (ht e1)
  | Eq (e1,e2) -> 1+ (max (ht e1) (ht e2))
  | Gt(e1,e2) -> 1+ (max (ht e1) (ht e2))
;;

(* size function for num nodes in the AST *)
let rec size e =  match e with 
  | Num n -> 1
  | Bl b->1
  | V x -> 1
  | Plus (e1, e2) -> 1 + (size e1) + (size e2)
  | Times (e1,e2) -> 1 + (size e1) + (size e2)
  | And (e1,e2) -> 1 + (size e1) + (size e2)
  | Or (e1,e2) -> 1 + (size e1) + (size e2)
  | Not e1 -> 1+ (size e1)
  | Eq (e1,e2) -> 1 + (size e1) + (size e2)
  | Gt(e1,e2) -> 1 + (size e1) + (size e2)
;;

(* Interpreting our abstract syntax into semantics : the eval function *)
type values = N of int | B of bool ;;

let rec eval e rho = match e with 
  | Num n -> N n
  | Bl b-> B (myBool2bool b)
  | V x -> rho x
  | Plus (e1,e2) -> let N n1 = (eval e1 rho ) and N n2 = (eval e2 rho ) in N (n1+ n2)
  | Times (e1,e2) -> let N n1 = (eval e1 rho ) and N n2 = (eval e2 rho ) in N (n1 * n2)
  | And (e1, e2)  -> let B b1 = (eval e1 rho) and B b2 = (eval e2 rho) in B (b1 && b2)
  | Or (e1, e2) -> let B b1 = (eval e1 rho) and B b2 = (eval e2 rho) in B (b1 || b2)
  | Not e1 -> let B b1 = (eval e1 rho) in B(not b1)
  | Eq (e1, e2)-> let N n1=(eval e1 rho ) and N n2=(eval e2 rho) in B (n1=n2)
  | Gt(e1,e2)-> let N n1=(eval e1 rho) and N n2=(eval e2 rho) in B (n1>n2)
;;
 (* Skipping type checking temporarily *)


(* Compiling expressions for a stack machine, creating a post order traversal of the AST encoded as OpCodes *)
type opcode = LDN of int | LDB of bool | LOOKUP of string | PLUS | TIMES | AND | OR | NOT | EQ | GT ;;

let rec compile e = match e with 
| Num n -> [LDN n]
| Bl b -> [LDB (myBool2bool b)]  (*Constants*)
| V x -> [LOOKUP x]     (* Variables*)
| Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
| Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
| And(e1, e2) -> (compile e1) @ (compile e2) @ [AND]
| Or (e1,e2) -> (compile e1) @ (compile e2) @ [OR]
| Not e1 -> (compile e1 ) @ [NOT]
| Eq (e1, e2)-> (compile e1) @ (compile e2)@[EQ]
| Gt (e1,e2)-> (compile e1)@(compile e2)@[GT]
;;

(* Encoding the Stack Machine ...*)

(* We first write an exception for when the Stack is empty/insufficient to execite the opcode list *)

exception Stuck of ( (string -> values) * values list * opcode list) ;;

let rec stkmc g s c = match s, c with
  | v::_ , [] -> v (* Return top when opcode list is empty *)
  | s, (LDN n)::c' -> stkmc g (N n::s) c'
  | s, (LDB b)::c' -> stkmc g (B b::s) c'
  | s, (LOOKUP x)::c' -> stkmc g ((g x)::s) c'
  | (N n2)::(N n1)::s', PLUS::c' -> stkmc g (N (n1 + n2)::s') c'
  | (N n2)::(N n1)::s', TIMES::c' -> stkmc g (N (n1 * n2)::s') c'
  | (B b2)::(B b1)::s', AND::c' -> stkmc g (B (b1 && b2)::s') c'
  | (B b2)::(B b1)::s', OR::c' -> stkmc g (B (b1 || b2)::s') c'
  | (B b1)::s', NOT::c' -> stkmc g (B (not b1)::s') c'
  | (N n2)::(N n1)::s', EQ::c' -> stkmc g (B (n1 = n2)::s') c'
  | (N n2)::(N n1)::s', GT::c' -> stkmc g (B (n1 > n2)::s') c'
  | _ , _ -> raise (Stuck (g, s, c))
;;