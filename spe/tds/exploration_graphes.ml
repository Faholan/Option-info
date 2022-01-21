(*
TD Exploration de graphes

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

type graphe = int list array ;;


(*Question 1*)
let taille = Array.length ;;

let n_arcs (grph: graphe) =
  let result = ref 0 in
    for i = 0 to Array.length grph - 1 do
      result := !result + List.length grph.(i)
    done ;
    !result
;;


(*Question 2*)
let exemple: graphe = [|
  [1; 4];
  [2];
  [3; 4];
  [];
  [2; 3]
|] ;;


(*Question 3*)
(*
Avec une structure de type FIFO, on effectue un parcours en largeur.
Avec une structure LIFO, c'est un parcours en profondeur
*)

(*Question 4*)
let traite_voisin accessibles stack sommet = match accessibles.(sommet) with
  | true -> ()
  | false -> accessibles.(sommet) <- true ;
    Stack.push sommet stack
;;

(*Question 5*)
let traite_sommet (grph: graphe) accessibles stack sommet =
  let rec traite_rec = function
    | [] -> ()
    | h::t -> traite_voisin accessibles stack h; traite_rec t
  in traite_rec grph.(sommet)
;;

(*Question 6*)
let accessibles grph sommet =
  let mat_acc = Array.make (taille grph) false and stack = Stack.create () in
    Stack.push sommet stack ;
    while not (Stack.is_empty stack) do
      traite_sommet grph mat_acc stack (Stack.pop stack)
    done ;
    mat_acc
;;

(*Question 7*)
(*
0 - 1 - 2 - 3 - 4
 \             /
  \          /
   5 - - - /
*)

(*Question 8*)
let traite_voisin_dist dist queue sommet (distance: int) = match dist.(sommet) with
  | Some _ -> ()
  | None -> (
    dist.(sommet) <- Some distance ;
    Queue.push sommet queue
  )
;;

let traite_sommet_dist (grph: graphe) dist queue sommet distance =
  let rec traite_rec = function
    | [] -> ()
    | h::t -> traite_voisin_dist dist queue h (distance + 1) ; traite_rec t
  in traite_rec grph.(sommet)
;;

let val_of_option = function
  | None -> failwith "val_of_option: None value"
  | Some x -> x
;;

let distances grph sommet =
  let mat_dist = Array.make (taille grph) None and queue = Queue.create () in
    Queue.push sommet queue ;
    mat_dist.(sommet) <- Some 0 ;
    while not (Queue.is_empty queue) do
      let cur_som = Queue.pop queue in
        traite_sommet_dist grph mat_dist queue cur_som (val_of_option mat_dist.(cur_som))
    done ;
    mat_dist
;;

(*Partie 4*)
type graphe_p = (int * int) list array ;;

(*Question 10*)
(*
1 - 2 - 3 - 4 - 5
| 1   1   1  1 |
|               |
| 3           7 |
| - - - 6 - - - |
*)

(*Question 11*)
(*
On peut utiliser une structure dans laquelle
on considère une structure de pile de priorité.
*)

(*Question 12*)

type priorite = int Stack.t array ref ;;

let create () = (ref [||]: priorite) ;;

let is_empty (arr: priorite) =
  let n = Array.length !arr in
    let rec _empty_rec = function
      | i when i = n -> true
      | i when Stack.is_empty !arr.(i) -> _empty_rec (i + 1)
      | _ -> false
    in _empty_rec 0
;;

let _lengthen (arr: priorite) length =
  let result = Array.make length (Stack.create ()) in
    for i = 0 to length - 1 do
      if i < Array.length !arr then
        result.(i) <- !arr.(i)
      else
        result.(i) <- Stack.create ()
      done ;
    arr := result
;;

let push distance sommet (arr: priorite) =
  if distance >= Array.length !arr then
    _lengthen arr (distance + 1)
  ;
  Stack.push sommet !arr.(distance)
;;

let pop (arr: priorite) =
  let n = Array.length !arr in
    let rec popper = function
      | i when i = n -> failwith "pop: Empty priorite"
      | i when Stack.is_empty !arr.(i) -> popper (i + 1)
      | i -> i, Stack.pop !arr.(i)
    in popper 0
;;

let traite_voisin_p dist priority sommet distance = match dist.(sommet) with
  | Some _ -> ()
  | None -> (
    dist.(sommet) <- Some distance ;
    push distance sommet priority
  )
;;

let traite_sommet_p (grph: graphe_p) dist priority sommet distance =
  let rec traite_rec = function
    | [] -> ()
    | (cur_dist, cur_som)::t -> traite_voisin_p dist priority cur_som (distance + cur_dist) ; traite_rec t
  in traite_rec grph.(sommet)
;;

(*Question 14*)

let distances_p grph sommet =
  let mat_dist = Array.make (taille grph) None and priority = create () in
    push 0 sommet priority ;
    mat_dist.(sommet) <- Some 0 ;
    while not (is_empty priority) do
      let cur_dist, cur_som = pop priority in
        traite_sommet_p grph mat_dist priority cur_som (val_of_option mat_dist.(cur_som))
    done ;
    mat_dist
;;

(*Question 15*)

let stable ens (grph: graphe) =
  let rec test_stable start = function
    | [] -> true
    | h::t when test_stable_elem h ens -> test_stable (h::start) t
    | _ -> false
  and test_stable_elem elem = function
    | [] -> true
    | h::t when h = elem -> test_stable_elem elem t
    | h::t when List.mem h grph.(elem) -> false
    | _::t -> test_stable_elem elem t
  in test_stable [] ens
;;

let clique ens (grph: graphe) =
  let rec test_clique start = function
    | [] -> true
    | h::t when test_clique_elem h ens -> test_clique (h::start) t
    | _ -> false
  and test_clique_elem elem = function
    | [] -> true
    | h::t when h = elem -> test_clique_elem elem t
    | h::t when List.mem h grph.(elem) -> test_clique_elem elem t
    | _ -> false
  in test_clique [] ens
;;

let rec list_of_ints = function
  | 0 -> [0]
  | n -> n::list_of_ints (n - 1)
;;

let complet (grph: graphe) = clique (list_of_ints (taille grph - 1)) grph ;;

let stable_max (grph: graphe) =
  let n = taille grph in
    let rec add_elem ens = function
      (*Ajoute un élément à ens si possible*)
      | i when i = n -> ens
      | i when List.mem i ens -> add_elem ens (i + 1)
      | i when stable (i::ens) grph -> let sol1 = add_elem (i::ens) (i + 1) and sol2 = add_elem ens (i + 1) in
        if List.length sol1 > List.length sol2 then
          sol1
        else
          sol2
      | i -> add_elem ens (i + 1)
    in
      add_elem [] 0
;;

let clique_max (grph: graphe) =
  let n = taille grph in
    let rec add_elem ens = function
      (*Ajoute un élément à ens si possible*)
      | i when i = n -> ens
      | i when List.mem i ens -> add_elem ens (i + 1)
      | i when clique (i::ens) grph -> let sol1 = add_elem (i::ens) (i + 1) and sol2 = add_elem ens (i + 1) in
        if List.length sol1 > List.length sol2 then
          sol1
        else
          sol2
      | i -> add_elem ens (i + 1)
    in
      add_elem [] 0
;;
