(* Custom Defining Library List Functions *)

(* Custom Length Function *)
let rec customlength l = match l with
  [] -> 0 |
  _::s -> 1+ customlength s 
;;


(* Custom Append Function *)
let rec customappend l1 l2 = match l1 with
  []-> l2 |
  x::y -> x:: (customappend y l2)
;;
(* Clearly, prefer cons since customappend is O(n) *)

(* List Reversal Quadratic Time *)

let rec revbrute l1 = match l1 with
  | [] -> [] 
  | x::y -> (revbrute y) @ [x]
;;

(* List Reversal O(n) time *)

let rev l1 =
  let rec revhelper l1 l2= match l1 with
    [] -> l2 |
    x::y -> revhelper y (x::l2)
  in
    revhelper l1 []
;;

(* Test the functions *)
let () =
  let list1 = [1; 2; 3; 4; 5] in
  let list2 = [6; 7; 8; 9; 10] in

  (* Test customlength *)
  Printf.printf "Length of list1: %d\n" (customlength list1);
  Printf.printf "Length of list2: %d\n" (customlength list2);

  (* Test customappend *)
  let appended_list = customappend list1 list2 in
  Printf.printf "Appended list: [";
  List.iter (fun x -> Printf.printf "%d; " x) appended_list;
  Printf.printf "]\n";

  (* Test revbrute *)
  let revbrute_list = revbrute list1 in
  Printf.printf "Reversed list using revbrute: [";
  List.iter (fun x -> Printf.printf "%d; " x) revbrute_list;
  Printf.printf "]\n";

  (* Test rev *)
  let rev_list = rev list1 in
  Printf.printf "Reversed list using rev: [";
  List.iter (fun x -> Printf.printf "%d; " x) rev_list;
  Printf.printf "]\n";
;;