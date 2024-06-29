let print_bool b = match b with 
    true -> print_endline "true" | 
    false -> print_endline "false" 
;;

(*Inductive Type Definition*)
type nat = Z | S of nat ;;


(*Convenient aliases*)
let zero =Z;;
let one = S Z;;
let two = S (S Z);;
let three = S(S (S Z));;

(*nat->bool = <fun>*)
let nonzero n = match n with 
    Z -> false |
    S _ -> true
;;


(*Conversion function to int which gives semantic meaning to our syntax*)
let rec nat2int n =  match n with 
    Z -> 0 |
    S x -> 1+(nat2int x)
;;

(*nat->nat=<fun>*)
let rec addnat m n = match m with 
    Z-> n |
    S x -> S (addnat x n)
;;

(*Use addnat to write multnat*)


let rec multnat m n = match m with 
    Z->Z |
    S x -> addnat n (multnat x n)
;;

(* expnat m n === m^n *)
let rec expnat m n = match n with 
    Z-> S Z |
    S x -> multnat m (expnat m x)
;;
(* assume 0^0 is 1 *)

