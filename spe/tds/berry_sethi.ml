(*Algorithme de Berry-Sethi

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

type 'a ensf = 'a list ;;

(*Question 1*)
let rec inter (l1: 'a ensf) (l2: 'a ensf) = match (l1, l2) with
  | ([], _) -> ([]: 'a ensf)
  | (_, []) -> []
  | (h1::t1, h2::t2) -> (
    if h1 < h2 then (
      inter t1 l2
    ) else (
      if h1 > h2 then
        inter l1 t2
      else
        h1::(inter t1 t2)
    )
  )
;;

(*Question 2*)
let rec union (l1: 'a ensf) l2 =
  match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (h1::t1, h2::t2) -> (
      if h1 < h2 then
        h1::(union t1 l2)
      else
        if h1 = h2 then (
          h1::(union t1 t2)
        ) else
          h2::(union l1 t2)
    )
;;

(*Question 3*)
let rec union_liste = function
  | [] -> []
  | [a] -> a
  | a::b::t -> union_liste ((union a b)::t)
;;

(*Question 4*)
let union_fun ens func =
  union_liste (List.fold_left (fun cur e -> (func e)::cur) [] ens)
;;

(*Question 5*)
let ensemble a = List.fold_left (fun cur e -> union [e] cur) [] a ;;

(*Question 6*)
let ajoute x ens = union [x] ens ;;

(*Question 7*)
let rec parties = function
  | ([]: 'a ensf) -> ([]: 'a ensf list)
  | h::t -> let current = parties t in
    List.fold_left (fun cur l -> (h::l)::cur) current current
;;

type ('a, 'b) automate = {
  alphabet: 'a ensf ;
  etats: 'b ensf ;
  initiaux: 'b ensf ;
  finaux: 'b ensf ;
  transitions: ('b * 'a * 'b) list ;
} ;;

type 'a mot = 'a list ;;

(*Question 8*)
let execute_lettre auto q x =
  let rec executor = function
    | [] -> []
    | (e, l, e2)::t when (e, l) = (q, x) -> ajoute e2 (executor t)
    | _::t -> executor t
  in executor auto.transitions
;;

(*Question 9*)
let rec execute auto etats = function
  | ([]: 'a mot) -> etats
  | h::t -> execute auto (union_fun etats (fun q -> execute_lettre auto q h)) t
;;

(*Question 10*)
let est_reconnu auto mot = inter auto.finaux (execute auto auto.initiaux mot) <> [] ;;

(*Question 11*)
let index elem =
  let rec _index cur = function
    | h::t when h = elem -> cur
    | h::t -> _index (cur + 1) t
    | [] -> failwith "Element not in list"
  in _index 0
;;

let rec in_common l1 = function
  | h::t when List.mem h l1 -> true
  | h::t -> in_common l1 t
  | [] -> false
;;

let rec determinise auto =
  let etats = ref [auto.initiaux] and transitions = ref [] and todo = Queue.create () in
    Queue.push auto.initiaux todo ;
    let rec treat () =
      if Queue.is_empty todo then
        ()
      else (
        let cur_etat = Queue.pop todo in
          let rec treat_chr = function
            | [] -> ()
            | h::t -> (
              let newstate = execute auto cur_etat [h] in
                if not (List.mem newstate !etats) then (
                  etats := newstate::!etats ;
                  Queue.push newstate todo
                ) ;
                transitions := (cur_etat, h, newstate)::!transitions ;
                treat_chr t
            )
          in
            treat_chr auto.alphabet ;
            treat () ;
      )
    in
      treat () ;
      {
        alphabet = auto.alphabet ;
        etats = !etats ;
        initiaux = [auto.initiaux] ;
        finaux = List.fold_left (
          fun current candidate -> if in_common candidate auto.finaux then
          candidate::current
            else
            current
        ) [] !etats ;
        transitions = List.fold_left (
          fun current (q1, a, q2) -> (q1, a, q2)::current
        ) [] !transitions
      }
;;

type 'a expr =
  | Zero
  | Un
  | Lettre of 'a
  | Conc of 'a expr * 'a expr
  | Etoile of 'a expr
  | Plus of 'a expr * 'a expr
;;

(*Question 12*)
let rec alphabet = function
  | Zero -> []
  | Un -> []
  | Lettre(a) -> [a]
  | Conc(a, b) -> union (alphabet a) (alphabet b)
  | Etoile(a) -> alphabet a
  | Plus(a, b) -> union (alphabet a) (alphabet b)
;;

(*Question 13*)
let rec contient_epsilon = function
  | Zero -> false
  | Un -> true
  | Lettre(_) -> false
  | Conc(a, b) -> contient_epsilon a && contient_epsilon b
  | Etoile(_) -> true
  | Plus(a, b) -> contient_epsilon a || contient_epsilon b
;;

(*Question 14*)
let pref1 a =
  let rec pref_flagged = function
    (*Add a flag if it contains ε*)
    | Zero -> [], false
    | Un -> [], true
    | Lettre(a) -> [a], false
    | Conc(a, b) -> let pre, flag = pref_flagged a in
      if flag then
        let pre2, flag2 = pref_flagged b in
          (union pre pre2), flag2
      else
        pre, false
    | Etoile(a) -> let pre, _ = pref_flagged a in
      pre, true
    | Plus(a, b) -> let pre1, flag1 = pref_flagged a and pre2, flag2 = pref_flagged b in
      (union pre1 pre2), flag1 || flag2
  in
    let pre, _ = pref_flagged a in
      pre
;;

let suf1 a =
  let rec suf_flagged = function
    | Zero -> [], false
    | Un -> [], true
    | Lettre(a) -> [a], false
    | Conc(a, b) -> let suf, flag = suf_flagged b in
      if flag then
        let suf2, flag2 = suf_flagged a in
          (union suf suf2), flag2
      else
        suf, false
    | Etoile(a) -> let suf, _ = suf_flagged a in
      suf, true
    | Plus(a, b) -> let suf1, flag1 = suf_flagged a and suf2, flag2 = suf_flagged b in
      (union suf1 suf2), flag1 || flag2
  in
    let suf, _ = suf_flagged a in
      suf
;;

(*Question 15*)
let rec match_flagged x = function
  | Zero -> false, false
  | Un -> false, true
  | Lettre(a) -> a = x, false
  | Conc(a, b) -> let match1, epsilon1 = match_flagged x a and match2, epsilon2 = match_flagged x b in
    (match1 && epsilon2) || (match2 && epsilon1), epsilon1 && epsilon2
  | Etoile(a) -> let match1, _ = match_flagged x a in
    match1, true
  | Plus(a, b) -> let match1, epsilon1 = match_flagged x a and match2, epsilon2 = match_flagged x b in
    match1 || match2, epsilon1 || epsilon2
;;

(*Question 16*)

let rec succ epr x = match epr with
  | Zero -> []
  | Un -> []
  | Lettre(_) -> []
  | Conc(a, b) -> let matches, epsilon = match_flagged x a in
    if matches && epsilon then
      union_liste [succ a x; pref1 b; succ b x]
    else
      if matches then
        union (succ a x) (pref1 b)
      else
        succ a x
  | Etoile(a) -> let matches, _ = match_flagged x a in
    if matches then
      ajoute x (succ a x)
    else
      succ a x
  | Plus(a, b) -> union (succ a x) (succ b x)
;;

(*Question 16*)
let rec first_transitions = function
  | [] -> []
  | h::t -> (None, h, Some(h))::(first_transitions t)
;;

let add_middle expr transitions alphabet =
  let rec adder_main transitions = function
    | [] -> transitions
    | h::t -> adder_main (adder_inner transitions h (succ expr h)) t
  and adder_inner transitions x = function
    | [] -> transitions
    | h::t -> adder_inner ((Some(x), h, Some(h))::transitions) x t
  in adder_main transitions alphabet
;;

let aut_local expr =
  let alpha = alphabet expr in
    {
      alphabet = alpha ;
      etats = None::(List.fold_right (fun x current -> Some(x)::current) alpha []) ;
      initiaux = [None] ;
      finaux = List.fold_left (fun current x -> Some(x)::current) (if contient_epsilon expr then [None] else []) (suf1 expr) ;
      transitions = add_middle expr (first_transitions (pref1 expr)) alpha
    }
;;

(*Question 17*)
let linearise expr =
  let rec linearisor flag = function
    | Zero -> Zero, flag
    | Un -> Un, flag
    | Lettre(a) -> Lettre((a, flag)), flag + 1
    | Conc(a, b) -> let newa, newflag = linearisor flag a in
      let newb, finalflag = linearisor newflag b in
        Conc(newa, newb), finalflag
    | Etoile(a) -> let newa, newflag = linearisor flag a in
      Etoile(newa), newflag
    | Plus(a, b) -> let newa, newflag = linearisor flag a in
    let newb, finalflag = linearisor newflag b in
      Plus(newa, newb), finalflag
  in
    let linearised, _ = linearisor 0 expr in
      linearised
;;

(*Question 18*)
let applique_morphisme auto func =
  {
    alphabet = List.fold_right (fun x current -> (func x)::current) auto.alphabet [] ;
    etats = auto.etats ;
    initiaux = auto.initiaux ;
    finaux = auto.finaux ;
    transitions = List.fold_right (fun (x, q, y) current -> (x, func q, y)::current) auto.transitions []
  }
;;

(*Question 19*)
let glushkov expr = applique_morphisme (aut_local (linearise expr)) (fun (x, _) -> x) ;;

let afd expr = determinise (glushkov expr) ;;
