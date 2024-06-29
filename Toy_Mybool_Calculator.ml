type myBool = T | F ;; 

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

let() = 
    let b1 = T in 
    let b2= F in 
    let not_b1 = myNot b1 in
    let not_b2 = myNot b2 in
    let and_b1_b2 = myAnd b1 b2 in
    let or_b1_b2 = myOr b1 b2 in

    print_string "b1:" ; print_myBool b1;
    print_string "b2:"; print_myBool b2;
    print_endline "myNot b1: ";
    print_myBool not_b1 ;
    print_endline "myNot b2 : ";
    print_myBool not_b2;
    print_endline "myAnd b1 b2 : ";
    print_myBool and_b1_b2;
    print_endline "myOr b1 b2 : ";
    print_myBool or_b1_b2;
;;


